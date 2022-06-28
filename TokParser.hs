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
  Cmd(..), CmpCmd(..), SmpCmd, Pipeline, AndOrList, SepList, IfClause, WhileLoop(..),
  clauses, else_part,
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
import Data.Functor

data Redirect = Redirect Token Fd String deriving (Eq,Show)
data SmpCmd = SmpCmd { redirects ::  [Redirect]
                     , assign :: [(String,String)]
                     , cmdWords :: [String]
                     } deriving(Eq, Show)
data IfClause = IfClause { clauses :: [(SepList, SepList)]
                         , else_part :: Maybe SepList
                         } deriving(Eq,Show)
data WhileLoop = WhileLoop SepList SepList deriving(Eq,Show)
data Cmd      = SCmd SmpCmd | CCmd CmpCmd [Redirect] deriving(Eq, Show)
--data Cmd    = Cmd SmpCmd | CmpCmd | (CmpCmd, [Redirect]) | FuncDef
data CmpCmd   = IfCmp IfClause | WhlCmp WhileLoop deriving(Eq,Show)
--data CmpCmd = CmpCmd BraceGroup | SubShell | For_Clause | Case_Clause | If_Clause

type Pipeline  = [Cmd]
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
--TODO should parse [(Token,SourcePos)] stream that remembers position in original source
nextPosW pos (Word w) xs  = incSourceColumn pos (length w +1)
nextPosTok pos x xs  = incSourceColumn pos 1

reservedWords :: [(String,Token)]
reservedWords = [("{", Lbrace)
                ,("}", Rbrace)
                ,("!", Bang)
                ,("in", In)
                ,("if",    If)
                ,("then",  Then)
                ,("else",  Else)
                ,("elif",  Elif)
                ,("fi",    Fi)
                ,("done",  Done)
                ,("do",    Do)
                ,("case",  Case)
                ,("esac",  Esac)
                ,("while", While)
                ,("until", Until)
                ,("for",   For)]

getToken :: String -> Maybe Token
getToken str = Map.lookup str (Map.fromList reservedWords) 

getWord :: TokParser String
getWord = tokenPrim show nextPosW ((>>= checkIfRes) . testWord)
  where checkIfRes w = guard (not $ w `elem` (fst <$> reservedWords)) $> w

resWord :: String -> TokParser Token
resWord tokStr = tokenPrim show nextPosW testIsTokStr
  where testIsTokStr w = testWord w >>= ($> tokStr) . guard . (==tokStr) >>= getToken

oneOfOp :: [Token] -> TokParser Token
oneOfOp ops = tokenPrim show nextPosTok checkTok
  where checkTok tok = guard (tok `elem` ops) $> tok

getAssignWord :: TokParser (String,String)
getAssignWord = tokenPrim show nextPosW ((>>= isAssignWord) . testWord)
  where isAssignWord = c2Maybe . (parse parseAssignWord "assignWord")
          where c2Maybe res = case res of Right t  -> Just t
                                          Left _   -> Nothing
        parseXBDName =           (++) . (:[]) <$> (letter <|> char '_')           <*> parseNameRest -- many (alphaNum <|> char '_')
          where parseNameRest = ((++) . (:[]) <$> (letter <|> char '_' <|> digit) <*> parseNameRest) <|> return ""
        parseAssignWord = (,) <$> parseXBDName <* char '=' <*> rest -- many anyChar
          where rest = (eof >> return "") <|> (++) . (:[]) <$> anyChar <*> rest

getIONr :: TokParser Int
getIONr = tokenPrim show nextPosW ((>>= Rd.readMaybe) . testWord)

getOp :: TokParser Token
getOp = tokenPrim show nextPosTok testOp

op :: Token -> TokParser ()
op tok = tokenPrim show nextPosTok (guard . (== tok))

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
        isRedirOp tok = guard (tok `elem` (fst <$> defaultFd)) $> tok

newLnList :: TokParser ()
newLnList = (tokenPrim show nextPosTok (guard . (==NEWLINE)) >> newLnList) <|> return ()

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
parsePipe = (fst <$>) <$> parseList (return . (,EOF)) True [PIPE]          parseCmd

parseCmd :: TokParser Cmd
parseCmd = (parseSmpCmd >>= return . SCmd) <|> CCmd <$> parseCmpCmd <*> many parseIORed

parseCmpCmd :: TokParser CmpCmd
parseCmpCmd = (parseIfClause "if" >>= return . IfCmp )
          <|> (parseWhileLoop     >>= return . WhlCmp)

parseAndOrList :: TokParser AndOrList
parseAndOrList =          parseList (return . (,EOF)) True [AND_IF, OR_IF] parsePipe

parseSepList :: Bool -> [Token] -> TokParser SepList
parseSepList nLs seps = parseList endCondition nLs seps parseAndOrList
  where endCondition e = ((e,) <$> oneOfOp seps <* when nLs newLnList) <|> return (e,EOF)
        lastSep = oneOfOp seps

parseCmpList :: TokParser SepList
parseCmpList = newLnList >> parseSepList True [SEMI,Ampersand,NEWLINE]

parseIfClause :: String -> TokParser IfClause
parseIfClause initKeyW = join $ handler <$> (resWord initKeyW *> parseCmpList) <*> (resWord "then" *> parseCmpList)
  where appendElif condition body cl_elif = return $ cl_elif {clauses = [(condition, body)] ++ clauses cl_elif}
        handler cond body = (                  parseIfClause "elif" >>= appendElif cond body)
                        <|> (resWord "else" >> parseCmpList <* resWord "fi" >>= return . (IfClause [(cond, body)]) . Just)
                        <|> (resWord "fi"   >> (return $ IfClause [(cond,body)] Nothing) )

parseWhileLoop :: TokParser WhileLoop
parseWhileLoop = WhileLoop <$> (resWord "while" *> parseCmpList) <*> doGroup

doGroup :: TokParser SepList
doGroup = resWord "do" *> parseCmpList <* resWord "done"

parseToks :: TokParser SepList
parseToks = (newLnList *> eofP *> return []) <|> (parseSepList False [SEMI,Ampersand] <* newLnList <* eofP)
  where eofP = eof <|> op EOF
-- parseSub :: TokParser SmpCmd
-- parseSub  = parseSmpCmd  <* (eof <|> op EOF)
