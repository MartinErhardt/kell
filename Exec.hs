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
 runSmpCmd,
 getDefaultShellEnv,
 expandWord,
 expandNoSplit,
 execSubShell,
 Shell,
 getVar
) where
import ShCommon
import WordExp
import Lexer
import TokParser (SmpCmd(..), parseToks, Redirect(..))

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Data.Stack
import qualified Data.List as L
import qualified Text.Read as Rd
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
import System.Posix.Types(Fd(..),FileMode)
import Data.Int(Int32(..))
import System.Posix.Files

runSmpCmd :: SmpCmd -> Shell (Maybe ProcessStatus)
runSmpCmd cmd = if cmdWords cmd /= [] then do
                  allFields <- foldl1 (\a b -> (++) <$> a <*> b)  (expandWord execSubShell True <$> cmdWords cmd)
                  if allFields /= [] then launchCmd (head allFields) (tail allFields) (execAssigns >> execRedirects)
                  else execLocal
                else execLocal
  where execAssigns =   if assign cmd /= []    then foldl1 (>>) (doAssign   <$> (assign cmd))    else return ()
        execRedirects = if redirects cmd /= [] then foldl1 (>>) (doRedirect <$> (redirects cmd)) else return ()
        execLocal = execAssigns >> execRedirects >> (return . Just $ Exited ExitSuccess)

getDefaultShellEnv :: ShellEnv
getDefaultShellEnv = ShellEnv (Map.fromList [("PS1","$ "), ("PS2","> ")]) ownerModes

execSubShell :: String -> Shell ()
execSubShell cmd = case toks of (Right val) -> case parse2Ast val of (Right ast) -> runSmpCmd ast >> return ()
                                                                     (Left err)  -> lift $ print err
                                (Left err)  -> lift $ print err
  where toks = parse lexer "subshell" cmd
        parse2Ast = parse parseToks "tokenstreamsubshell"

launchCmd :: FilePath -> [String] -> Shell () -> Shell (Maybe ProcessStatus)
launchCmd cmd args prepare = do 
  curEnv <- get
  forkedPId <- lift . forkProcess $ evalStateT (prepare >> lift runInCurEnv) curEnv >> return ()
  lift $ getProcessStatus True True forkedPId
  where runInCurEnv = getEnvironment >>= (executeFile cmd True args) . Just


doAssign :: (String,String) -> Shell ()
doAssign (name,word) = (expandNoSplit execSubShell word) >>= putVar name

doRedirect :: Redirect -> Shell ()
doRedirect (Redirect tok fd path) = get >>= lift . (getAction tok (Fd $ fromIntegral fd) path) . shFMode >>= return . return ()
  where truncOFlag  = (OpenFileFlags False False False False True)
        appendOFlag = (OpenFileFlags True  False False False False)
        redirAction = [(LESS,      (\fd path m -> openFd path ReadOnly  (Just m) truncOFlag  >>= (flip dupTo $ fd)))
                      ,(GREAT,     (\fd path m -> openFd path WriteOnly (Just m) truncOFlag  >>= (flip dupTo $ fd)))
                      ,(DGREAT,    (\fd path m -> openFd path WriteOnly (Just m) appendOFlag >>= (flip dupTo $ fd)))
                      ,(LESSGREAT, (\fd path m -> openFd path ReadWrite (Just m) truncOFlag  >>= (flip dupTo $ fd)))
                      ,(LESSAND,   (\fd1 fd2 m -> dupTo fd1 (Fd . fromIntegral $ Rd.read fd2) ) )
                      ,(GREATAND,  (\fd1 fd2 m -> dupTo fd1 (Fd . fromIntegral $ Rd.read fd2) ) )]
        getAction tok = (\(Just v) -> v) $ Map.lookup tok (Map.fromList redirAction) -- Pattern matching failure for Here-Documenents

