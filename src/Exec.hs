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
 getVar
) where
import ShCommon
import WordExp
import Lexer
import TokParser (SmpCmd(..), Pipeline, AndOrList, SepList, Cmd(..), CmpCmd(..), IfClause(..), WhileLoop(..), Redirect(..))
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
import System.Posix.Types(ProcessID, Fd(..),FileMode)
import Data.Int(Int32(..))
import System.Posix.Files
import System.Exit
import Foreign.C.Error

runSmpCmd :: SmpCmd -> Shell ExitCode
runSmpCmd cmd = if cmdWords cmd /= [] then do
                  allFields <- foldl1 (\a b -> (++) <$> a <*> b)  (expandWord execCmd True <$> cmdWords cmd)
                  if allFields /= [] then launchCmd (head allFields) (tail allFields) (prep ())
                  else prep ExitSuccess
                else prep ExitSuccess
  where execAssigns =   when (assign    cmd /= []) ( foldl1 (>>) (doAssign   <$> (assign cmd))    >> return () )
        execRedirects = when (redirects cmd /= []) ( foldl1 (>>) (doRedirect <$> (redirects cmd)) >> return () )
        prep arg = execAssigns >> execRedirects >> return arg

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
  where printDiag msg = (liftIO . putStrLn $ "kell: " ++ msg)
        handleCmdErr exit = do
          ia <- interactive <$> (lift get)
          case exit of ExpErr msg            -> printDiag msg >> if ia then (return $ ExitFailure 1) else throwE exit
                       CmdNotFoundErr msg ec -> printDiag msg >> if ia then (return ec) else throwE exit
        runCCmd cmd redirs = do
          ioReversals <- foldl (\a1 a2 -> (flip (>>)) <$> a1 <*> a2) (return $ return stdOutput) (doRedirect <$> redirs)
          exitCode <- catchE (runCmpCmd cmd) handleCmdErr
          liftIO $ ioReversals >> return exitCode

runCmpCmd :: CmpCmd -> Shell ExitCode
runCmpCmd (IfCmp clause) = runIfClause  clause
runCmpCmd (WhlCmp loop ) = runWhileLoop loop

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

getDefaultShellEnv :: Bool -> IO ShellEnv
getDefaultShellEnv interactive = do
  envVars <- ( ((\(name,val) -> (name,(val,True)) ) <$> ) <$> getEnvironment)
  foldl (flip $ (>>) . (\(name, (val,exp)) -> if exp then setEnv name val else return ())) (return ()) preDefined
  return $ ShellEnv (Map.fromList $ envVars ++ preDefined) interactive ownerModes
  where preDefined = [("PS1",  ("$ ",  False))
                     ,("PS2",  ("> ",  False))
                     ,("SHELL",("kell",True ))]

exec :: TokParser a -> (a -> Shell ExitCode) -> String -> Shell ExitCode
exec parser executor cmd = case toks of (Right val) -> case parse2Ast val of (Right ast) -> executor ast
                                                                             (Left err)  -> liftIO $ errorMsg err
                                        (Left err)  -> liftIO $ errorMsg err
  where toks :: Either ParseError [Token]
        toks = parse lexer "subshell" cmd
        parse2Ast tokens = parse parser "tokenstreamsubshell" tokens
        errorMsg err = print err >> (return $ ExitFailure 1)

execCmd :: String -> Shell ExitCode
execCmd = exec parseSub runSepList

waitToExitCode :: ProcessID -> Shell ExitCode
waitToExitCode pid = do
  state <- liftIO $ getProcessStatus True False pid
  case state of (Just (Exited exitCode) ) -> return exitCode
                _                         -> waitToExitCode pid

launchCmd :: FilePath -> [String] -> Shell () -> Shell ExitCode
launchCmd cmd args prepare = do 
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

doAssign :: (String,String) -> Shell ()
doAssign (name,word) = (expandNoSplit execCmd word) >>= putVar name

doRedirect :: Redirect -> Shell (IO Fd)
doRedirect (Redirect tok fd path) = do expandedPath <- expandNoSplit execCmd path
                                       lift get >>= liftIO . (getAction tok fd expandedPath) . shFMode
  where truncOFlag   = (OpenFileFlags False False False False True)
        noFlag       = (OpenFileFlags False False False False False)
        appendOFlag  = (OpenFileFlags True  False False False False)
        change2Fd fd = (<*) <$> (flip dupTo) fd <*> closeFd
        restoreOpen fd action = dup fd <* (action >>= change2Fd fd) >>= return . (change2Fd fd)
        str2Fd = Fd . fromIntegral . Rd.read
        -- handle Redirection errors
        redirAction = [(LESS,      (\fd path m -> restoreOpen fd (openFd path ReadOnly  (Just m) noFlag      )))
                      ,(GREAT,     (\fd path m -> restoreOpen fd (openFd path WriteOnly (Just m) truncOFlag  ))) --TODO case file exists and noclobber opt
                      ,(CLOBBER,   (\fd path m -> restoreOpen fd (openFd path WriteOnly (Just m) truncOFlag  )))
                      ,(DGREAT,    (\fd path m -> restoreOpen fd (openFd path WriteOnly (Just m) appendOFlag )))
                      -- TODO DLESS
                      ,(LESSGREAT, (\fd path m -> restoreOpen fd (openFd path ReadWrite (Just m) noFlag      )))
                      ,(LESSAND,   (\fd1 fd2 m -> dupTo (str2Fd fd2) fd1 >>= return . ((flip dupTo) (str2Fd fd2)) ))
                      ,(GREATAND,  (\fd1 fd2 m -> dupTo fd1 (str2Fd fd2) >>= return . dupTo fd ))]
        getAction tok = (\(Just v) -> v) $ Map.lookup tok (Map.fromList redirAction) -- Pattern matching failure for Here-Documenents

