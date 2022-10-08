--   Copyright 2022 Martin Erhardt
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.
{-# LANGUAGE ScopedTypeVariables #-}

module Exec
(
 runPipe, runSmpCmd, runAndOr, runSepList,
 getDefaultShellEnv,
 expandWord, expandNoSplit,
 execCmd,
 Shell,
 getVar,
 putFunc
) where
import ShCommon
import WordExp
import Lexer
import ShCommon (SmpCmd(..), FuncDef(..), Pipeline, AndOrList, SepList, Cmd(..), CmpCmd(..), IfClause(..), WhileLoop(..), Redirect(..))
import TokParser

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Data.Stack
import qualified Data.List as L
import qualified Text.Read as Rd
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import System.Posix.Process
import System.Posix.IO
import System.Posix.IO(OpenFileFlags(..))
import System.IO
import System.Exit
import Control.Monad
import GHC.IO.Exception(IOException(..))
import GHC.IO.Exception(IOErrorType(..))
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import System.Posix.Signals
import System.Environment
import Data.Bits
import System.Posix.Internals
import System.Posix.Types(ProcessID, Fd(..),FileMode)
import Data.Int(Int32(..))
import System.Posix.Files
import System.Exit
import Foreign.C.Error
import OpenAI

offerCompletion :: String -> [String] -> Shell ExitCode
offerCompletion _ (arg1:rest) = do
  completedCmd <- (liftIO . fetchCompletion) arg1
  liftIO $ putStrLn completedCmd
  execCmd completedCmd

runSmpCmd :: SmpCmd -> Shell ExitCode
runSmpCmd cmd = if cmdWords cmd /= [] then do
                  env <- lift get
                  allFields <- foldl1 (\a b -> (++) <$> a <*> b)  (expandWord execCmd True <$> cmdWords cmd)
                  if allFields /= [] then 
                    case getF allFields env of Just cmd -> pushArgs (tail allFields) *> runCmd cmd <* popArgs
                                               _ -> getCmd allFields
                  else prep ExitSuccess
                else prep ExitSuccess
  where getF l env = Map.lookup (head l) (func env)
        execAssigns =   when (assign    cmd /= []) ( foldl1 (>>) (doAssign   <$> (assign cmd))    >> return () )
        execRedirects = when (redirects cmd /= []) ( foldl1 (>>) (doRedirect <$> (redirects cmd)) >> return () )
        prep arg = execAssigns >> execRedirects >> return arg
        changePosArgs modifier curEnv  = lift . put $ curEnv {posArgs = modifier curEnv}
        pushArgs args = lift get >>= changePosArgs (((flip stackPush) args)  . posArgs)
        stackPopIfNotEmpty s = case stackPop s of Just st  -> fst st
                                                  Nothing -> s
        popArgs = lift get >>= changePosArgs (stackPopIfNotEmpty . posArgs)
        builtinCmd = Map.fromList [("gpt3", offerCompletion)]
        getCmd (cmd:args) = case Map.lookup cmd builtinCmd of Just builtin -> builtin cmd args
                                                              Nothing -> launchCmd args (prep ()) cmd

runIfClause :: IfClause -> Shell ExitCode
runIfClause cl = do
  res <- runSepList . fst . head $ clauses cl
  if res == ExitSuccess then runSepList . snd . head $ clauses cl
  else if (length . clauses) cl/= 1 then (runIfClause    $ cl{ clauses = tail $ clauses cl})
  else case else_part cl of Just body -> runSepList body
                            Nothing   -> return ExitSuccess

runWhileLoop :: WhileLoop -> Shell ExitCode
runWhileLoop (WhileLoop cond body) = runSepList cond >>= handler ExitSuccess
  where handler bodyEC nextEC = if nextEC == ExitSuccess then restLoop else return bodyEC
        restLoop = join $ handler <$> runSepList body <*> runSepList cond

runCmd :: Cmd -> Shell ExitCode
runCmd cmdSym = case cmdSym of SCmd cmd        -> catchE (runSmpCmd cmd) handleCmdErr
                               CCmd cmd redirs -> runCCmd cmd redirs
                               FCmd f          -> runFuncDef f
  where printDiag msg = (liftIO . putStrLn $ "kell: " ++ msg)
        handleCmdErr exit = do
          ia <- interactive <$> (lift get)
          case exit of ExpErr msg            -> printDiag msg >> if ia then return () else throwE exit
                       RedirUErr msg         -> printDiag msg
                       -- this can happen in command substitutions
                       SyntaxErr msg         -> printDiag msg >> if ia then return () else throwE exit
                       CmdNotFoundErr msg ec -> printDiag msg >> if ia then return () else throwE exit
          return $ getErrExitCode exit
        runCCmd cmd redirs = do
          ioReversals <- foldl (\a1 a2 -> (flip (>>)) <$> a1 <*> a2) (return $ return stdOutput) (doRedirect <$> redirs)
          exitCode <- catchE (runCmpCmd cmd) handleCmdErr
          liftIO $ ioReversals >> return exitCode

runCmpCmd :: CmpCmd -> Shell ExitCode
runCmpCmd (IfCmp clause) = runIfClause  clause
runCmpCmd (WhlCmp loop ) = runWhileLoop loop
runCmpCmd (BrGroup list) = runSepList list

runFuncDef :: FuncDef -> Shell ExitCode
runFuncDef f = putFunc f >> return ExitSuccess

runPipe :: Pipeline -> Shell ExitCode
runPipe pipeline = if length pipeline == 1 then runCmd . head $ pipeline else do
  pipes          <- liftIO $ sequence [createPipe | n <- [1..length pipeline-1]]
  createChildren <- lift get >>= sequence . ( (liftIO . forkProcess) <$>) . (finalActions pipes)
  liftIO $ sequence ((\(fd1,fd2) -> closeFd fd1 >> closeFd fd2) <$> pipes)
  waitToExitCode . last $ createChildren -- ksh style ...
  where fds = [createPipe | n <- [1..((length pipeline)-1)]]
        doRedMid ((in1,out1), (in2,out2)) = do
          dupTo in1 stdInput
          dupTo out2 stdOutput
          closeFd out1 >> closeFd in2 >> closeFd in1 >> closeFd out2
        doRedBeg (inFd,outFd) = dupTo outFd stdOutput >> closeFd inFd  >> closeFd outFd
        doRedEnd (inFd,outFd) = dupTo inFd stdInput   >> closeFd outFd >> closeFd inFd
        createRedirL pL = [doRedBeg (head pL)] ++ (doRedMid <$> (\l -> zip l $ tail l) pL) ++ [doRedEnd (last pL)]
        runAction ev redirs pipeA = redirs >> evalStateT (runExceptT $ runCmd pipeA) ev >> return ()
        finalActions ps ev = zipWith (runAction ev) (createRedirL ps) pipeline

runAndOr :: AndOrList -> Shell ExitCode
runAndOr [(pipe,Lexer.EOF)] = runPipe pipe
runAndOr andOrL = case head andOrL of (p,AND_IF) -> runPipe p >>= dropIf (/=ExitSuccess)
                                      (p,OR_IF)  -> runPipe p >>= dropIf (==ExitSuccess)
    where dropIf cond ec = if cond ec then 
                                 if rest andOrL /= []  then runAndOr $ rest andOrL
                                 else return ec
                               else runAndOr $ tail andOrL
          rest = tail . dropWhile (( == (snd $ head andOrL)) . snd)

runSepList :: SepList -> Shell ExitCode
runSepList [] = return ExitSuccess
runSepList sepL = case head sepL of (andOrL, Ampersand) -> runAsync andOrL >> return ExitSuccess >>= continueWith sepL
                                    (andOrL, _)         -> runAndOr andOrL >>= continueWith sepL
   --TODO store PID in ShellEnv; close stdInput in async child
  where runAsync andOrL = lift get >>= liftIO . forkProcess . (>> return ()) . evalStateT (runExceptT $ runAndOr andOrL)
        continueWith l ec = if tail l /= [] then (runSepList $ tail l) else return ec

-- |The 'getDefaultShellEnv' function generates a initial shell environment at program launch. 
-- This includes the import of environment variables and program arguments as well as the definition of prompt variables.
-- TODO inbuilt functions
getDefaultShellEnv :: [String] -- ^ script arguments soon to be positional parameters
 -> Bool -- ^ interactive mode
 -> IO ShellEnv -- ^ Resulting shell environment containing all environment variables and functions.
getDefaultShellEnv args interactive = do
  envVars <- ( ((\(name,val) -> (name,(val,True)) ) <$> ) <$> getEnvironment)
  foldl (flip $ (>>) . (\(name, (val,exp)) -> if exp then setEnv name val else return ())) (return ()) preDefined
  -- liftIO $ print args
  return $ ShellEnv (Map.fromList $ envVars ++ preDefined) (stackPush stackNew args) Map.empty interactive ownerModes
  where preDefined = [("PS1",  ("$ ",  False))
                     ,("PS2",  ("> ",  False))
                     ,("SHELL",("kell",True ))]

-- |The 'exec' function parses a string cmd with a parser into an ast node of type a.
-- It then executes said ast with the function 'executor', which generates a shell action based on this ast.
exec :: TokParser a -- ^ parser
  -> (a -> Shell ExitCode) -- ^ function generating a shell action based on ast
  -> String -- ^ command to parse and execute
  -> Shell ExitCode -- ^ resulting shell action that can be executed with 
exec parser executor cmd = case toks of (Right val) -> case parse2Ast val of (Right ast) -> executor ast
                                                                             (Left err)  -> (throwE . SyntaxErr) (show err)
                                        (Left err)  -> (throwE . SyntaxErr) (show err)
  where toks :: Either ParseError [Token]
        toks = parse lexer "subshell" cmd
        parse2Ast tokens = parse parser "tokenstreamsubshell" tokens

execCmd :: String -> Shell ExitCode
execCmd = exec parseSub runSepList

-- |The 'waitToExitCode' function waits until the process given by pId terminates with a exit code.
-- This is necessary because getProcessStatus and on a lower lever waitpid(2) also return on signal calls.
waitToExitCode :: ProcessID-- ^ process to wait for 
 -> Shell ExitCode -- ^ final exit code
waitToExitCode pid = do
  state <- liftIO $ getProcessStatus True False pid
  case state of (Just (Exited exitCode) ) -> return exitCode
                _                         -> waitToExitCode pid

-- |This function creates a shell action, executing the program given at the path 'cmd' with the arguments 'args'.
-- Before executing the program and after the fork it executes the shell action 'prepare'.
launchCmd :: [String] -- ^ arguments
  -> Shell () -- ^ shell action to be executed right before program
  -> FilePath -- ^ location of program to launch
  -> Shell ExitCode -- ^ exit code of launched program
launchCmd args prepare cmd = do
  forkedPId <- lift get >>= liftIO . forkProcess . (>> return ()) . evalStateT (runExceptT $ prepare >> liftIO runInCurEnv)
  e <- waitToExitCode forkedPId
  case e of ExitFailure 126 -> throwE $ CmdNotFoundErr (cmd ++ ": (Permission denied)")         e
            ExitFailure 127 -> throwE $ CmdNotFoundErr (cmd ++ ": (No such file or directory)") e
            ExitFailure 125 -> throwE $ CmdNotFoundErr (cmd ++ ": (Unknown)") e
            _               -> return e
  where execHandler (Left (e :: IOException)) = case ioe_type e of PermissionDenied  -> exitImmediately $ ExitFailure 126
                                                                   NoSuchThing       -> exitImmediately $ ExitFailure 127
                                                                   _                 -> exitImmediately $ ExitFailure 125
        runUnchecked = getEnvironment >>= (executeFile cmd True args) . Just 
        runInCurEnv = (Control.Exception.try $ runUnchecked) >>= execHandler

-- |The 'doAssign' function expands a word and assigns it to a variable
doAssign :: (String,String) -- ^ tuple containing the name of the program in the first component and the word to assign in its second one 
  -> Shell () -- ^ executable shell action
doAssign (name,word) = (expandNoSplit execCmd word) >>= putVar name

-- |The 'doRedirect' performs the redirectionS in a given Redirect.
-- It then returns an IO action wrapped in the Shell monad, that reverts said redirection.
doRedirect :: Redirect -- ^redirectionS to perform
  -> Shell (IO Fd) -- ^IO action, that reverts the given redirectionS
doRedirect (Redirect tok fd path) = do liftIO $ print fd
                                       expandedPath <- expandNoSplit execCmd path
                                       lift get >>= (getAction tok fd expandedPath) . shFMode
  where truncOFlag   = (OpenFileFlags False False False False True)
        noFlag       = (OpenFileFlags False False False False False)
        appendOFlag  = (OpenFileFlags True  False False False False)
        change2Fd fd = (<*) <$> (flip dupTo) fd <*> closeFd
        -- if fd exists duplicate fd to a intermediate filedescriptor
        restoreOpenEx fd action = dup fd <* (action >>= change2Fd fd ) >>= return . (change2Fd fd)
        -- if fd does not exist, we can just directly use fd
        restoreOpenNoEx fd action = (action >>= change2Fd fd) >> (return $ closeFd fd >> return fd)
        -- TODO only catch dup fd
        restoreOpen fd action = liftIO $ catch (restoreOpenEx fd action) (\(_ :: IOException) -> restoreOpenNoEx fd action)
        redirFds fd1 fd2 modes = do
          mode <- liftIO $ fdGetMode fd2
          if mode `elem` modes then (liftIO $ dupTo fd1 (Fd fd2)) >> (return $ (dupTo (Fd fd2) fd1))
                               else throwE . RedirUErr $ (show fd2) ++ ": (Permission denied)"
        -- handle Redirection errors
        redirAction = [(LESS,      (\fd path m -> restoreOpen fd (openFd path ReadOnly  (Just m) noFlag      )))
                      ,(GREAT,     (\fd path m -> restoreOpen fd (openFd path WriteOnly (Just m) truncOFlag  ))) --TODO case file exists and noclobber opt
                      ,(CLOBBER,   (\fd path m -> restoreOpen fd (openFd path WriteOnly (Just m) truncOFlag  )))
                      ,(DGREAT,    (\fd path m -> restoreOpen fd (openFd path WriteOnly (Just m) appendOFlag )))
                      -- TODO DLESS
                      ,(LESSGREAT, (\fd path m -> restoreOpen fd (openFd path ReadWrite (Just m) noFlag      )))
                      ,(LESSAND,   (\fd1 fd2 m -> redirFds fd1 (fromIntegral $ Rd.read fd2) [ReadMode, ReadWriteMode] ))
                      ,(GREATAND,  (\fd1 fd2 m -> redirFds fd1 (fromIntegral $ Rd.read fd2) [WriteMode, AppendMode, ReadWriteMode] ))]
        getAction tok = (\(Just v) -> v) $ Map.lookup tok (Map.fromList redirAction) -- Pattern matching failure for Here-Documenents
