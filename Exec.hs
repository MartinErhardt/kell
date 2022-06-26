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
import TokParser (SmpCmd(..), Pipeline, AndOrList, SepList, Redirect(..))
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
import Control.Monad.Trans.Class
import System.Posix.Signals
import System.Environment
import Data.Bits
import System.Posix.Types(ProcessID, Fd(..),FileMode)
import Data.Int(Int32(..))
import System.Posix.Files
import System.Exit

runSmpCmd :: SmpCmd -> Shell ExitCode
runSmpCmd cmd = if cmdWords cmd /= [] then do
                  allFields <- foldl1 (\a b -> (++) <$> a <*> b)  (expandWord execCmd True <$> cmdWords cmd)
                  if allFields /= [] then launchCmd (head allFields) (tail allFields) (execAssigns >> execRedirects)
                  else execLocal
                else execLocal
  where execAssigns =   if assign cmd    /= [] then foldl1 (>>) (doAssign   <$> (assign cmd))    else return ()
        execRedirects = if redirects cmd /= [] then foldl1 (>>) (doRedirect <$> (redirects cmd)) else return ()
        execLocal = execAssigns >> execRedirects >> (return ExitSuccess)

runPipe :: Pipeline -> Shell ExitCode
runPipe pipeline = if length pipeline == 1 then runSmpCmd . head $ pipeline else do
  pipes          <- lift $ sequence [createPipe | n <- [1..length pipeline-1]]
  createChildren <- get >>= sequence . ( (lift . forkProcess) <$>) . (finalActions pipes)
  lift $ sequence ((\(fd1,fd2) -> closeFd fd1 >> closeFd fd2) <$> pipes)
  waitToExitCode . last $ createChildren -- ksh style ...
  where fds = [createPipe | n <- [1..((length pipeline)-1)]]
        doRedMid ((in1,out1), (in2,out2)) = do
          dupTo in1 stdInput
          dupTo out2 stdOutput
          closeFd out1 >> closeFd in2 >> closeFd in1 >> closeFd out2
        doRedBeg (inFd,outFd) = dupTo outFd stdOutput >> closeFd inFd  >> closeFd outFd
        doRedEnd (inFd,outFd) = dupTo inFd stdInput   >> closeFd outFd >> closeFd inFd
        createRedL pL = [doRedBeg (head pL)] ++ (doRedMid <$> (\l -> zip l $ tail l) pL) ++ [doRedEnd (last pL)]
        finalActions ps ev = zipWith (\redirs pipeA -> redirs >> evalStateT (runSmpCmd pipeA) ev >> return () ) (createRedL ps) pipeline

runAndOr :: AndOrList -> Shell ExitCode
runAndOr [(pipe,EOF)] = runPipe pipe
runAndOr andOrL = case head andOrL of (p,AND_IF) -> runPipe p >>= dropIf (/=ExitSuccess)
                                      (p,OR_IF)  -> runPipe p >>= dropIf (==ExitSuccess)
    where dropIf cond ec = if cond ec then 
                                 if rest andOrL /= []  then runAndOr $ rest andOrL
                                 else return ec
                               else runAndOr $ tail andOrL
          rest = tail . dropWhile (( == (snd $ head andOrL)) . snd)

runSepList :: SepList -> Shell ExitCode
runSepList sepL = case head sepL of (andOrL, Ampersand) -> runAsync andOrL >> return ExitSuccess >>= continueWith sepL
                                    (andOrL, _)         -> runAndOr andOrL >>= continueWith sepL
   --TODO store PID in ShellEnv; close stdInput in async child
  where runAsync andOrL = get >>= lift . forkProcess . (>> return ()) . evalStateT (runAndOr andOrL)
        continueWith l ec = if tail l /= [] then (runSepList $ tail l) else return ec

getDefaultShellEnv :: IO ShellEnv
getDefaultShellEnv = do
  envVars <- ( ((\(name,val) -> (name,(val,True)) ) <$> ) <$> getEnvironment)
  foldl (flip $ (>>) . (\(name, (val,exp)) -> if exp then setEnv name val else return ())) (return ()) preDefined
  return $ ShellEnv (Map.fromList $ envVars ++ preDefined) ownerModes
  where preDefined = [("PS1",  ("$ ",  False))
                     ,("PS2",  ("> ",  False))
                     ,("SHELL",("kell",True ))]

exec :: TokParser a -> (a -> Shell ExitCode) -> String -> Shell ExitCode
exec parser executor cmd = case toks of (Right val) -> case parse2Ast val of (Right ast) -> executor ast
                                                                             (Left err)  -> lift $ errorMsg err
                                        (Left err)  -> lift $ errorMsg err
  where toks :: Either ParseError [Token]
        toks = parse lexer "subshell" cmd
        parse2Ast tokens = parse parser "tokenstreamsubshell" tokens
        errorMsg err = print err >> (return $ ExitFailure 1)

execCmd :: String -> Shell ExitCode
execCmd = exec parseSmpCmd runSmpCmd

waitToExitCode :: ProcessID -> Shell ExitCode
waitToExitCode pid = do
  state <- lift $ getProcessStatus True False pid
  case state of (Just (Exited exitCode) ) -> return exitCode
                _                         -> waitToExitCode pid

launchCmd :: FilePath -> [String] -> Shell () -> Shell ExitCode
launchCmd cmd args prepare = do 
  curEnv <- get
  forkedPId <- lift . forkProcess $ evalStateT (prepare >> lift runInCurEnv) curEnv
  waitToExitCode forkedPId
  where runInCurEnv = getEnvironment >>= (executeFile cmd True args) . Just

doAssign :: (String,String) -> Shell ()
doAssign (name,word) = (expandNoSplit execCmd word) >>= putVar name

change2Fd fd newFd = dupTo newFd fd >> closeFd newFd

doRedirect :: Redirect -> Shell ()
doRedirect (Redirect tok fd path) = do expandedPath <- expandNoSplit execCmd path
                                       get >>= lift . (getAction tok fd expandedPath) . shFMode
  where truncOFlag  = (OpenFileFlags False False False False True)
        noFlag      = (OpenFileFlags False False False False False)
        appendOFlag = (OpenFileFlags True  False False False False)
        -- handle Redirection errors
        redirAction = [(LESS,      (\fd path m -> openFd path ReadOnly  (Just m) noFlag      >>= change2Fd fd))
                      ,(GREAT,     (\fd path m -> openFd path WriteOnly (Just m) truncOFlag  >>= change2Fd fd)) --TODO case file exists and noclobber opt
                      ,(CLOBBER,   (\fd path m -> openFd path WriteOnly (Just m) truncOFlag  >>= change2Fd fd))
                      ,(DGREAT,    (\fd path m -> openFd path WriteOnly (Just m) appendOFlag >>= change2Fd fd))
                      -- TODO DLESS
                      ,(LESSGREAT, (\fd path m -> openFd path ReadWrite (Just m) noFlag      >>= change2Fd fd))
                      ,(LESSAND,   (\fd1 fd2 m -> dupTo fd1 (Fd . fromIntegral $ Rd.read fd2) >> return () ) )
                      ,(GREATAND,  (\fd1 fd2 m -> dupTo fd1 (Fd . fromIntegral $ Rd.read fd2) >> return () ) )]
        getAction tok = (\(Just v) -> v) $ Map.lookup tok (Map.fromList redirAction) -- Pattern matching failure for Here-Documenents

