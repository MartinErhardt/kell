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
 Shell,
 getVar
) where
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

data ShellEnv = ShellEnv { var :: Map.Map String String
                      -- , func :: Map.Map String String
                         , shFMode :: FileMode
                         } deriving(Eq, Show)
type Shell = StateT ShellEnv IO

runSmpCmd :: SmpCmd -> Shell (Maybe ProcessStatus)
runSmpCmd cmd = if cmdWords cmd /= [] then do
                  allFields <- foldl1 (\a b -> (++) <$> a <*> b)  (expandWord True <$> cmdWords cmd)
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

launchCmdSub :: String -> Shell String
launchCmdSub cmd = do
  pipe      <- lift createPipe
  curEnv    <- get
  forkedPId <- lift . forkProcess $ do
    (fdToHandle . fst $ pipe) >>= hClose
    dupTo (snd pipe) stdOutput
    evalStateT (execSubShell cmd) curEnv
    return ()
  lift $ (fdToHandle . snd $ pipe) >>= hClose
  lift $ getProcessStatus False False forkedPId -- TODO Error handling
  lift $ ( (fdToHandle . fst $ pipe) >>= hGetContents >>= return . reverse . dropWhile (=='\n') . reverse)

doAssign :: (String,String) -> Shell ()
doAssign (name,word) = (expandNoSplit word) >>= putVar name

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

getVar :: String -> Shell (Maybe String)
getVar name =     get >>= return . (Map.lookup name) . var 

putVar :: String -> String -> Shell ()
putVar name val = get >>= return . changeNamespace ( (Map.insert name val) . var) >>= put
  where changeNamespace modifier curEnv = ShellEnv (modifier curEnv) (shFMode curEnv)
-----------------------------------------------Word Expansion--------------------------------------------------------------
type Field = String

expandParams :: String -> Shell String
expandParams name = getVar name >>= return . handleVal
  where handleVal var =  case var of (Just val) -> val -- set and not null
                                     (Nothing)  -> ""  -- unset

fieldExpand :: String -> Shell [Field]
fieldExpand s = getVar "IFS" >>= return . ifsHandler >>= (flip parse2Shell) s . split2F 
  where ifsHandler var = case var of (Just val) -> val
                                     (Nothing)  -> " \t\n"

split2F :: String -> Parser [Field]
split2F ifs = (eof >> return [""])
          <|> ( (\c fs -> [ [c] ++ head fs] ++ tail fs) <$> noneOf ifs <*> split2F ifs )
          <|> (                                  oneOf ifs >> ([""]++) <$> split2F ifs )

escapeUnorigQuotes :: Parser String
escapeUnorigQuotes = (eof >> return "") <|> handle <$> anyChar <*> escapeUnorigQuotes
  where handle c str =  (if c `elem` "\xfe\xff\"'\\" then ['\xfe',c] else [c]) ++ str

removeEscReSplit :: Parser [Field]
removeEscReSplit = (eof >> return [""])
                <|> (char '\xff' >> removeEscReSplit >>= return . ([""]++) )
                <|> (char '\xfe' >> appendC <$> anyChar <*> removeEscReSplit )
                <|> (               appendC <$> anyChar <*> removeEscReSplit )
  where appendC c rest = [[c] ++ head rest] ++ tail rest

removeQuotes :: Parser String
removeQuotes = (eof >> return "")
           <|> (char '\''   >> (++) <$> quoteEsc escapes       (char '\'' >> return "") <*> removeQuotes )
           <|> (char '"'    >> (++) <$> quoteEsc dQuoteRecCond (char '"'  >> return "") <*> removeQuotes )
           <|> (char '\\'   >> (++) <$> (anyChar >>= return . (:[]) )                   <*> removeQuotes )
           <|> (char '\xfe' >> (++) <$> (anyChar >>= return . (['\xfe']++) . (:[]) )    <*> removeQuotes )
           <|> (               (++) <$> (anyChar >>= return . (:[]) )                   <*> removeQuotes )
  where escapes = (++) <$> (escape '\xfe' <|> (char '\\' >> anyChar >>= return . (:[])) ) 
        dQuoteRecCond = escapes <|> ((++) <$> getDollarExp id stackNew)

parse2Shell :: (Show a) => Parser a -> String -> Shell a
parse2Shell parser = return . (\(Right v) -> v) . parse parser "string"

data ExpType = NoExp | ParamExp | CmdExp | ArithExp deriving (Eq,Show,Enum)
data ExpTok = ExpTok { expTyp :: ExpType
                     , fSplit :: Bool 
                     , expStr :: String} deriving(Eq, Show)

parseExps :: Bool -> [String] -> Parser [ExpTok]
parseExps doFExps names = (eof >> return [])
        <|> (char '$'  >>((++) . (:[]) . (ExpTok ParamExp doFExps)       <$> getVExp names <*> parseExps doFExps names
                      <|> (++) . (:[]) . (ExpTok ParamExp doFExps)       <$> getParamExp   <*> parseExps doFExps names 
                      <|> (++) . (:[]) . (ExpTok CmdExp   doFExps)       <$> getCmdSubExp  <*> parseExps doFExps names ) )
        <|> (char '`'  >> (++) . (:[]) . (ExpTok CmdExp   doFExps)       <$> bQuoteRem     <*> parseExps doFExps names )
        <|> (char '\'' >> (++) . (:[]) . (ExpTok NoExp False) . enc '\'' <$> quoteRem      <*> parseExps doFExps names )
        <|> (char '"'  >> (++) . processDQuote                           <$> dQuoteRem     <*> parseExps doFExps names )
        <|>               (++) . (:[]) . (ExpTok NoExp False)            <$> readNoExp     <*> parseExps doFExps names
  where enc c = (++[c]) . ([c]++)
        readNoExp = (++) <$> (escape '\\' <|> (noneOf "$`'\"" >>= return . (:[])) ) <*> (readNoExp <|> return "")

        quoteRem =  quote  (char '\'' >> return "")
        dQuoteRem = dQuote (char '"'  >> return "") 
        bQuoteRem = quoteEsc ( (++) <$> (char '\\' >> (((char '\\' <|> char '$' <|> char '`') >> anyChar >>= return . (:[]) ) 
                                                  <|> (anyChar >>= return . (['\\']++) . (:[]) ) ) ) ) (char '`' >> return "")
        getVExp :: [String] -> Parser String
        getVExp names = foldl1 (<|>) ((\a -> try $ string a >> return a ) <$> names )
        getCmdSubExp = char '(' >> (quote $ getDollarExp (\_ -> "") (stackPush stackNew ")") )
        getParamExp  = char '{' >> (quote $ getDollarExp (\_ -> "") (stackPush stackNew "}") )
        processDQuote = enc (ExpTok NoExp False "\"") . (\(Right v) -> v) . parse (parseExps False names) "qE"

execExp :: ExpTok -> Shell [Field]
execExp (ExpTok NoExp _ s) = return [s]
execExp (ExpTok t split s) = case t of CmdExp   -> launchCmdSub s
                                       ParamExp -> expandParams s
                         >>= return . (\(Right v) -> v) . parse escapeUnorigQuotes "EscapeUnorigQuotes"
                         >>= if split then fieldExpand else return . (:[])

expandWord :: Bool -> String -> Shell [Field]
expandWord split cmdWord = do
  env  <- get
  exps <- parse2Shell (parseExps split $ names env) cmdWord
  -- lift $ print exps 
  fs   <- foldl1 (\a1 a2 -> ppJoinEnds <$> a1 <*> a2) (execExp <$> exps)
-- TODO expandedPNFields <- Pathname expansion
-- how to exploit my shell? on command substitution print to stdout ['\0','\0', 'f'] -> profit
-- expand split2F to accept [String] aa field seperator
  -- lift $ print fs
  noQuotes <- parse2Shell removeQuotes ( concat . L.intersperse ['\xff'] $ filter (/="") fs)
  -- lift $ putStrLn noQuotes
  res <- parse2Shell removeEscReSplit noQuotes-- TODO expandedPNFields <- Pathname expansion
  -- lift $ print res
  return res
  where names env = fst <$> (Map.toList $ var env)
        ppJoinEnds l1 l2 = init l1 ++ [last l1 ++ head l2] ++ tail l2

expandNoSplit :: String -> Shell Field
expandNoSplit cmdWord = expandWord False cmdWord >>= return . head
