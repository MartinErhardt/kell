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
module TokParser
(
  parseToks,
  parseSmpCmd,
  TokParser,
  SmpCmd, Pipeline, AndOrList, SepList,
  cmdWords,
  assign,
  Redirect(Redirect),
  redirects
) where
import qualified Text.Read as Rd
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Pos
import Text.Parsec.String
import Lexer (Token (..))
import System.IO
import Data.Maybe
import System.Posix.IO
import System.Posix.Types(Fd(..))
import qualified Data.Map as Map
import Control.Monad

data Redirect = Redirect Token Fd String deriving (Eq,Show)
data SmpCmd = SmpCmd { redirects ::  [Redirect]
                     , assign :: [(String,String)]
                     , cmdWords :: [String]
                     } deriving(Eq, Show)
--data Cmd    = Cmd SmpCmd | CmpCmd | (CmpCmd, [Redirect]) | FuncDef
--data CmpCmd = CmpCmd BraceGroup | SubShell | For_Clause | Case_Clause | If_Clause

type Pipeline  = [SmpCmd]
type AndOrList = [(Pipeline, Token)] -- Last Token has no meaning
type SepList   = [(AndOrList,Token)]

type TokParser = Parsec [Token] ()

testWord :: Token -> Maybe String
testWord tok = case tok of Word w -> Just w
                           _      -> Nothing
testOp :: Token -> Maybe Token
testOp tok = case tok of Word w   -> Nothing
                         NEWLINE  -> Nothing
                         EOF      -> Nothing
                         t        -> Just t
--TODO should parse [(Token,SourcePos)] stream that remembers position in original soource
nextPosW pos (Word w) xs  = incSourceColumn pos (length w +1)
nextPosTok pos x xs  = incSourceColumn pos 1

reservedWords :: [(String,Token)]
reservedWords = [("{", Lbrace)
                ,("}", Rbrace)
                ,("!", Bang)
                ,("in", In)]

getToken :: String -> Maybe Token
getToken str = Map.lookup str (Map.fromList reservedWords) 

getWord :: TokParser String
getWord = tokenPrim show nextPosW ((>>= checkIfRes) <$> testWord)
  where checkIfRes w = if w `elem` (fst <$> reservedWords) then Nothing else Just w

oneOfOp :: [Token] -> TokParser Token
oneOfOp ops = tokenPrim show nextPosTok checkTok
  where checkTok tok = if tok `elem` ops then Just tok else Nothing

getAssignWord :: TokParser (String,String)
getAssignWord = tokenPrim show nextPosW ((>>= isAssignWord) <$> testWord)
  where isAssignWord = c2Maybe . (parse parseAssignWord "assignWord")
          where c2Maybe res = case res of Right t  -> Just t
                                          Left _   -> Nothing
        parseXBDName =           (++) . (:[]) <$> (letter <|> char '_')           <*> parseNameRest
          where parseNameRest = ((++) . (:[]) <$> (letter <|> char '_' <|> digit) <*> parseNameRest) <|> return ""
        parseAssignWord = (,) <$> parseXBDName <* char '=' <*> rest
          where rest = (eof >> return "") <|> (++) . (:[]) <$> anyChar <*> rest

getIONr :: TokParser Int
getIONr = tokenPrim show nextPosW (\w -> testWord w >>= Rd.readMaybe)

getOp :: TokParser Token
getOp = tokenPrim show nextPosTok testOp

op :: Token -> TokParser ()
op tok = tokenPrim show nextPosTok (\t -> if t == tok then Just () else Nothing)

parseIORed :: TokParser Redirect
parseIORed = ((\op -> Redirect op (getDefOp op) ) <$> getRedirOp <*> getWord) <|> do
  n       <- getIONr
  redirOp <- getRedirOp
  file    <- getWord
  return $ Redirect redirOp (Fd . fromIntegral $ n) file
  where getRedirOp = tokenPrim show nextPosTok ((>>= isRedirOp) <$> testOp)
        defaultFd = [(LESS,     stdInput)
                    ,(GREAT,    stdOutput)
                    ,(CLOBBER,  stdOutput)
                    ,(DLESS,    stdInput)
                    ,(DGREAT,   stdOutput)
                    ,(LESSAND,  stdInput)
                    ,(GREATAND, stdOutput)
                    ,(LESSGREAT,stdInput)]
        getDefOp = (\(Just v) -> v) . (flip Map.lookup) (Map.fromList defaultFd)
        isRedirOp tok = if tok `elem` (fst <$> defaultFd) then Just tok else Nothing

newLnList :: TokParser ()
newLnList = (tokenPrim show nextPosTok getNewline >> newLnList) <|> return ()
  where getNewline w = case w of NEWLINE -> Just ()
                                 _       -> Nothing

parseSmpCmd :: TokParser SmpCmd
parseSmpCmd = addRedirect <$> try parseIORed <*> (parseSmpCmd <|> base)
          <|> addAssign   <$> getAssignWord  <*> (parseSmpCmd <|> base)
          <|> addWord     <$> getWord        <*>  parseSmpCmdSuf
  where base = return $ SmpCmd [] [] []
        addRedirect redir cmd = cmd {redirects = [redir]  ++ redirects cmd}
        addAssign strstr cmd =  cmd {assign    = [strstr] ++ assign cmd   }
        addWord str cmd =       cmd {cmdWords  = [str]    ++ cmdWords cmd }
        parseSmpCmdSuf = addRedirect <$> try parseIORed <*> parseSmpCmdSuf
                     <|> addWord     <$> getWord        <*> parseSmpCmdSuf
                     <|> base

parseList :: (a -> TokParser (a,Token)) -> Bool -> [Token] -> TokParser a -> TokParser [(a,Token)]
parseList endCondition nLs sepToks parseElem = parseElem 
  >>= (<|>) <$> try . recList <*> ((:[]) <$>) . endCondition
  where rest = parseList endCondition nLs sepToks parseElem
        addE elem sep = ([(elem,sep)] ++)
        recList elem = addE elem <$> oneOfOp sepToks <* when nLs newLnList <*> rest

parsePipe :: TokParser Pipeline
parsePipe = (fst <$>) <$> parseList (return . (,EOF)) True [PIPE]          parseSmpCmd

parseAndOrList :: TokParser AndOrList
parseAndOrList =          parseList (return . (,EOF)) True [AND_IF, OR_IF] parsePipe

parseSepList :: TokParser SepList
parseSepList = parseList endCondition False seps parseAndOrList
  where endCondition e = ((e,) <$> oneOfOp seps) <|> return (e,EOF)
        seps = [SEMI, Ampersand]
        lastSep = oneOfOp seps

parseToks :: TokParser SepList
parseToks = parseSepList <* (eof <|> op EOF)

parseCmd :: TokParser SmpCmd
parseCmd  = parseSmpCmd  <* (eof <|> op EOF)
