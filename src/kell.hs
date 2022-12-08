{-# LANGUAGE CPP #-}

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
--  limitations under the License.
-- {-# LANGUAGE TupleSections #-}
import ShCommon
import ShCommon(ShellError(..))
import Lexer
import TokParser
import Exec (Shell)
import Exec

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (
#if MIN_VERSION_transformers(0,6,0)
  tryE
#else
  catchE, runExceptT, throwE, ExceptT
#endif
  )
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import qualified Control.Exception as Ex

import System.IO
import System.IO.Error
import System.Environment
import System.Exit
import System.Posix.Process

import Data.Functor
import Data.Char
import qualified Data.List as L
import qualified Data.Text as Txt

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Pos
import Text.Parsec.Pos (SourcePos(..))
import Text.Parsec.Error
import Text.Parsec.Error(ParseError(..))
import Text.Parsec.Error(Message(..))

data Origin = FromFile{ commandFile    :: String}
            | FromOp  { commandStr     :: String
                      , commandName    :: Maybe(String)}
            | FromStdIn | None deriving(Eq,Show)
-- data Option = Interactive deriving(Eq,Show)
data PArgs = PArgs { opts        :: String
                   , script      :: Origin
                   , args        :: [String] }deriving (Eq,Show)
type ArgError = String

main :: IO ()
main = do
  args  <- getArgs
  pArgs <- handlePArgs $ parse parseArgs "args" args
  shEnv <- getDefaultShellEnv args $ ('i' `elem` opts pArgs) || (script pArgs == None)
  res <- evalStateT (runExceptT $ execInterpreter pArgs) shEnv >>= exitHandler
  print res
  exitImmediately res
  where handlePArgs pArgs = case pArgs of Right pa -> return pa
                                          Left e   -> print e >> (exitWith $ ExitFailure 1) >> (return $ PArgs "" None [])
        exitHandler :: Either ShellError ExitCode -> IO ExitCode
        exitHandler exit = case exit of Right ec -> return ec
                                        Left e   -> return $ getErrExitCode e

type ArgParser = Parsec [String] ()

parseArgs :: ArgParser PArgs
parseArgs = do
  opts <- concat <$> (many $ tokenPrim show incPos checkOpt)
  orig <- parseOrigin $ getOrigOpt opts
  many arg >>= return . (PArgs opts orig)
  where getOrigOpt allOpts = let origOpts = L.intersect allOpts "cs"
                             in if origOpts == [] then 'z' else head origOpts
        parseOrigin opt = case opt of 'c' -> FromOp <$> name <*> ((Just <$> name) <|> return Nothing)
                                      's' -> return FromStdIn
                                      'z' -> (name >>= (return . FromFile) ) <|> return None
        checkOpt w = if          head w == '-' then Just $ tail w             -- TODO check if lowercase
                     else guard (head w == '+') $>  (toUpper <$> tail w)
        incPos pos x xs = incSourceColumn pos 1
        name    = tokenPrim show incPos (\w -> guard ((head w /= '+') && (head w /= '-')) $> w)
        arg     = tokenPrim show incPos Just

execInterpreter :: PArgs -> Shell ExitCode
execInterpreter args =
  case script args of FromFile path    -> do
                                            handle   <- liftIO $ openFile path ReadMode
                                            exitCode <- interprete (getLn handle) ExitSuccess
                                            liftIO $ hClose handle
                                            return exitCode
                      FromOp str name  -> interpreteCmd noMoreLn str
                      -- start interactive if stdin empty
                      FromStdIn        -> interprete (getLn stdin) ExitSuccess
                      None             -> interprete (getLn stdin) ExitSuccess
  where noMoreLn = return . Left $ userError "end of input"
        getLn handle = Ex.try $ ((++"\n") <$> hGetLine handle) -- dont pp if EOF

printPrompt :: String -> Shell ()
printPrompt var = liftIO (hFlush stdout) >> expandNoSplit execCmd var >>= liftIO . putStr >> liftIO (hFlush stdout)

interprete :: IO (Either IOError String) -> ExitCode -> Shell ExitCode
interprete lineGetter lastEC = do
  ia <- (lift get) >>= return . interactive
  when ia (printPrompt "$PS1") >> liftIO lineGetter >>= handleFetch ia
  where handleExec ia res = case res of Right ec -> interprete lineGetter ec
                                        Left  e  -> if ia then interprete lineGetter (getErrExitCode $ SyntaxErr (show e))
                                                          else throwE $ SyntaxErr (show e)
        escNLn cmd = if last cmd == '\\' then tail $ tail cmd else cmd
        handleFetch ia lnew = case lnew of Right s -> tryE (interpreteCmd lineGetter (escNLn s)) >>= handleExec ia
                                           Left  e -> return lastEC
#if !MIN_VERSION_transformers(0,6,0)
        tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
        tryE m = catchE (liftM Right m) (return . Left)
#endif

interpreteCmd :: IO (Either IOError String) -> String -> Shell ExitCode
interpreteCmd lineGetter curCmd = do
  ia <- interactive <$> (lift get)
  case toks of Right v -> case parse2AST v of Right ast -> runSepList ast
                                              Left e    -> handleErrs ia "EOF" e
               Left e  -> handleErrs ia "eof" e
  where parse2AST   = parse parseToks "tokenstream"
        toks        = parse lexer "charstream" curCmd
        incompleteFetch eOld oldLn newLn = case newLn of Right s -> interpreteCmd lineGetter (oldLn++s)
                                                         Left e  -> throwE $ SyntaxErr (show eOld)
        incomplete ia e str     = when ia (printPrompt "$PS2") >> liftIO lineGetter >>= incompleteFetch e str
        handleErrs ia eofT e = if [eofT,""] `L.intersect` (messageString <$> errorMessages e) /= []
                                 then incomplete ia e curCmd
                               else (liftIO $ print e ) >> (throwE $ SyntaxErr (show e))
