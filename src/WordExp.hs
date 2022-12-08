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
{-# LANGUAGE TupleSections #-}
module WordExp(
  expandWord,
  expandNoSplit
) where

import ExpArith
import Patterns
import ShCommon

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import Data.Functor ((<&>))
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Data.Stack
import System.Exit
import System.IO
import System.Posix.IO
import System.Posix.Process
import Text.Parsec
import Text.Parsec.String (Parser)

type Field = String

launchCmdSub :: (String -> Shell ExitCode) -> String -> Shell String
launchCmdSub launcher cmd = do
  pipe      <- liftIO createPipe
  curEnv    <- lift get
  forkedPId <- liftIO . forkProcess $ do
    closeFd . fst $ pipe
    dupTo (snd pipe) stdOutput
    --FIXME 'echo $(if;)' parse2Ast in parent
    res <- evalStateT (runExceptT $ launcher cmd) curEnv
    case res of Right ec -> exitImmediately ec
                Left err -> (exitImmediately . getErrExitCode) err
    return ()
  liftIO $ closeFd . snd $ pipe
  liftIO $ getProcessStatus False False forkedPId -- TODO Error handling
  liftIO $ (fdToHandle . fst $ pipe) >>= hGetContents <&> L.dropWhileEnd (== '\n')

expandParams :: (String -> Shell ExitCode) -> String -> Shell String
expandParams launcher toExp = do
  handleParseOutput $ parse parseParamExp "parameter expansion" toExp
  where parseSplit = foldl1 (<|>) ( try . string . fst <$> actionT)
        parseParamExp :: Parser (String, String, String)
        parseParamExp =  try ((, , ) <$> (parseXBDName <|> many1 digit) <*> parseSplit <*> many anyChar)
              <|> ((,"-","") <$>   (parseXBDName <|> many1 digit) <* eof)
        assignW p w = putVar p w >> return w
        first (x ,_, _) = x
        sec   (_, x, _) = x
        third (_ ,_ ,x) = x
        actionT :: [(String, (String -> String -> String, String -> String -> Shell String, String -> String -> Shell String))]
        actionT = [("-",  (const, \p w -> return w, \p w -> return w ))
                  ,("-",  (const, \p w -> return "", \p w -> return w ))
                  ,(":=", (const, assignW,             assignW ))
                  ,("=",  (const, \p w -> return "", assignW ))
                  ,(":?", (const, raiseExpErr vnull,   raiseExpErr vunset )) --TODO throw error
                  ,("?",  (const, assignW,             raiseExpErr vunset )) --TODO throw error
                  ,(":+", (\p w -> w, \p w -> return "", \p w -> return ""))
                  ,("+",  (\p w -> w, \p w -> return w,  \p w -> return ""))]
        extractAction split f = case Map.lookup split (Map.fromList actionT) of Just triple -> f triple
        applyAction :: String -> String -> Maybe String -> String -> Shell String
        applyAction split name p w = case p of Nothing   -> extractAction split third name w
                                               Just ""   -> extractAction split sec name w
                                               Just sub  -> return $ extractAction split first sub w
        expandTo (p, split, w) = join $ applyAction split p <$> getVar p <*> expandNoSplit launcher w
        vnull  = "parameter null"
        vunset = "parameter not set"
        raiseExpErr msg p w = throwE . ExpErr $ p ++ ": " ++ msg -- bash for unit tests
        handleParseOutput p = case p of Right triple -> expandTo triple
                                        Left e       -> throwE . ExpErr $ toExp ++ ": syntax error: " ++ show e

fieldExpand :: String -> Shell [Field]
fieldExpand s = getVar "IFS" >>= flip parse2Shell s . split2F . ifsHandler
  where ifsHandler = fromMaybe " \t\n"

split2F :: String -> Parser [Field]
split2F ifs = (eof >> return [""])
          <|> ( (\c fs -> (c : head fs) : tail fs) <$> noneOf ifs <*> split2F ifs )
          <|> (                        oneOf ifs >> ([""]++) <$> split2F ifs )

escapeUnorigQuotes :: Parser String
escapeUnorigQuotes = (eof >> return "") <|> handle <$> anyChar <*> escapeUnorigQuotes
  where handle c str =  (if c `elem` "\xfe\xff\"'\\" then ['\xfe',c] else [c]) ++ str

removeEscReSplit :: Parser [Field]
removeEscReSplit = (eof >> return [""])
                <|> (char '\xff' >> removeEscReSplit <&> ([""]++) )
                <|> (char '\xfe' >> appendC <$> anyChar <*> removeEscReSplit )
                <|> (               appendC <$> anyChar <*> removeEscReSplit )
  where appendC c rest = (c : head rest) :  tail rest

removeQuotes :: Parser String
removeQuotes = (eof >> return "")
           <|> (char '\''   >> (++) <$> quoteEsc escapes       (char '\'' >> return "") <*> removeQuotes )
           <|> (char '"'    >> (++) <$> quoteEsc dQuoteRecCond (char '"'  >> return "") <*> removeQuotes )
           <|> (char '\\'   >> (++) <$> (anyChar <&> (:[]) )                            <*> removeQuotes )
           <|> (char '\xfe' >> (++) <$> (anyChar <&> (['\xfe']++) . (:[]) )             <*> removeQuotes )
           <|> (               (++) <$> (anyChar <&> (:[]) )                            <*> removeQuotes )
  where escapes = (++) <$> (escape '\xfe' <|> ((char '\\' >> anyChar) <&> (:[])))
        dQuoteRecCond = escapes <|> ((++) <$> getDollarExp id stackNew)

parse2Shell :: (Show a) => Parser a -> String -> Shell a
parse2Shell parser = return . (\(Right v) -> v) . parse parser "string"

data ExpType = NoExp | ParamExp | CmdExp | ArithExp deriving (Eq,Show,Enum)
data ExpTok = ExpTok { expTyp :: ExpType
                     , fSplit :: Bool
                     , expStr :: String} deriving(Eq, Show)

parseExps :: Bool -> [String] -> Parser [ExpTok]
parseExps doFExps names = (eof >> return [])
        <|> (char '$' >> ((++) . (:[]) . ExpTok ParamExp doFExps       <$> getVExp names    <*> parseExps doFExps names
                      <|> (++) . (:[]) . ExpTok ParamExp doFExps       <$> getExp "{" "}"   <*> parseExps doFExps names
                      <|> (++) . (:[]) . ExpTok ArithExp doFExps       <$> getExp "((" "))" <*> parseExps doFExps names
                      <|> (++) . (:[]) . ExpTok CmdExp   doFExps       <$> getExp "(" ")"   <*> parseExps doFExps names
                      <|> (++) . (:[]) . ExpTok ParamExp doFExps       <$> parseXBDName     <*> parseExps doFExps names) )
        <|> (char '`'  >> (++) . (:[]) . ExpTok CmdExp   doFExps       <$> bQuoteRem        <*> parseExps doFExps names )
        <|> (char '\'' >> (++) . (:[]) . ExpTok NoExp False . enc '\'' <$> quoteRem         <*> parseExps doFExps names )
        <|> (char '"'  >> (++) . processDQuote                         <$> dQuoteRem        <*> parseExps doFExps names )
        <|>               (++) . (:[]) . ExpTok NoExp False            <$> readNoExp        <*> parseExps doFExps names
  where enc c = (++[c]) . ([c]++)
        readNoExp = (++) <$> (escape '\\' <|> (noneOf "$`'\"" <&> (:[])) ) <*> (readNoExp <|> return "")

        quoteRem =  quote  (char '\'' >> return "")
        dQuoteRem = dQuote (char '"'  >> return "")
        bQuoteRem = quoteEsc ( (++) <$> (char '\\' >> ((((char '\\' <|> char '$' <|> char '`') >> anyChar) <&> (:[]) )
                                                  <|> (anyChar <&> (['\\']++) . (:[]) ) ) ) ) (char '`' >> return "")
        -- the expansion shall use the longest valid name (see XBD Name)
        namesSorted = L.sortBy (\a b -> compare (length b) (length a)) names
        getVExp :: [String] -> Parser String
        getVExp names = many1 digit <|> foldl1 (<|>) ((\a -> try $ string a >> return a ) <$> namesSorted )
        getExp begin end = try (string begin) >> quote (getDollarExp (const "") (stackPush stackNew end) )
        processDQuote = enc (ExpTok NoExp False "\"") . (\(Right v) -> v) . parse (parseExps False names) "qE"

execExp :: (String -> Shell ExitCode) -> ExpTok -> Shell [Field]
execExp launcher (ExpTok NoExp _ s) = return [s]
execExp launcher (ExpTok t split s) =
  case t of
    CmdExp   -> launchCmdSub  launcher s
    ParamExp -> expandNoSplit launcher s >>= expandParams launcher
    ArithExp -> expandNoSplit launcher s >>= expandArith
  >>= (if split then fieldExpand else return . (:[])) . (\(Right v) -> v) . parse escapeUnorigQuotes "EscapeUnorigQuotes"

expandWord :: (String -> Shell ExitCode) -> Bool -> String -> Shell [Field]
expandWord launcher split ""      = return [""]
expandWord launcher split cmdWord = do
  env  <- lift get
  exps <- parse2Shell (parseExps split $ names env) cmdWord
  -- lift $ print exps
  fs   <- foldl1 (\a1 a2 -> ppJoinEnds <$> a1 <*> a2) (execExp launcher <$> exps)
-- TODO expandedPNFields <- Pathname expansion
-- how to exploit my shell? on command substitution print to stdout ['\0','\0', 'f'] -> profit
-- expand split2F to accept [String] aa field seperator
  fsPathExpanded <- liftIO $ concat <$> mapM expandPath fs
  -- liftIO $ print fsPathExpanded
  -- lift $ print fs
  noQuotes <- parse2Shell removeQuotes ( L.intercalate ['\xff'] $ filter (/="") fsPathExpanded)
  -- lift $ putStrLn noQuotes
  parse2Shell removeEscReSplit noQuotes-- TODO expandedPNFields <- Pathname expansion
  where names env = fst <$> Map.toList (var env)
        ppJoinEnds l1 l2 = init l1 ++ [last l1 ++ head l2] ++ tail l2

expandNoSplit :: (String -> Shell ExitCode) -> String -> Shell Field
expandNoSplit launcher cmdWord = expandWord launcher False cmdWord <&> head
