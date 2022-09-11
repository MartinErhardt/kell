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

module ExpArith(
  expandArith
)where
import ShCommon(ShellError(..))
import ShCommon

import Data.Bits
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Text.Read as Rd

type AParser a = ParsecT String () Shell a
uOpMap = [[("~", complement)]
         ,[("!", fromEnum . not . (>0) )] ]
bOpMap = [[("*",  (*))]
         ,[("/",  div)]
         ,[("%",  mod)]
         ,[("+",  (+))]
         ,[("-",  (-))]
         ,[("<<", shift)]
         ,[(">>", flip shift)]
         ,[("<",  fE (<)), ("<=", fE (<=))]
         ,[(">",  fE (>)), (">=", fE (>=))]
         ,[("==", fE (==))]
         ,[("!=", fE (/=))]
         ,[("&",  (.&.))]
         ,[("^",  xor)]
         ,[("|",  (.|.))]
         ,[("&&", b2I (&&))]
         ,[("||", b2I (||))]]
  where b2I f i1 i2 = fromEnum $ f (i1>0) (i2>0)
        fE  f i1 i2 = fromEnum $ f i1     i2

lexer :: Tok.GenTokenParser String () Shell
lexer = Tok.makeTokenParser style
  where bOps = concat . fst . unzip $ (unzip <$> bOpMap)
        uOps = concat . fst . unzip $ (unzip <$> uOpMap)
        style = Lang.emptyDef
         {Tok.commentStart    = ""
         ,Tok.commentEnd      = ""
         ,Tok.commentLine     = ""
         ,Tok.identStart      = letter   <|> char '_'
         ,Tok.identLetter     = alphaNum <|> char '_'
         ,Tok.opStart         = Tok.opLetter style
         ,Tok.opLetter        = oneOf (concat $ bOps ++ uOps)
         ,Tok.reservedOpNames = bOps++uOps
         ,Tok.reservedNames   = []
         ,Tok.caseSensitive   = True
         }

numb :: AParser Int
numb = Tok.natural lexer >>= return . fromIntegral

getVal :: String -> AParser Int
getVal name = (lift . getVar) name >>= handleVar
  where handleVar v = case v of (Just v) -> case Rd.readMaybe v of (Just n) -> return n
                                                                   _        -> unexpected $ name ++ " not a number"
                                _        -> return 0 -- unexpected $ name ++ " unset"

getVarVal :: AParser Int
getVarVal = Tok.identifier lexer >>= getVal

getOp :: (String, a) -> AParser a
getOp (op, f) = Tok.reservedOp lexer op >> return f

assignExp :: AParser Int
assignExp = do
  toAssign <- Tok.identifier lexer
  opFunc   <- (foldl1 (<|>) (getOp <$> assignOps))
  oldval   <- getVal toAssign
  newval   <- expr >>= return . opFunc oldval
  lift $ putVar toAssign (show $ newval)
  return newval
  where addEq = fmap (\(op,f) -> (op ++ "=",f))
        assignOps = (++[("=",curry snd)]) . addEq . filter (\(s,_) -> s/= ">" && s/="<") $ concat bOpMap

baseExpr :: AParser Int
baseExpr = Tok.parens lexer expr <|> try(assignExp) <|> getVarVal <|> numb

expr :: AParser Int
expr = Ex.buildExpressionParser table baseExpr
  where table =( (((flip Ex.Infix $ Ex.AssocLeft) . getOp)  <$>) <$> bOpMap) ++ (((Ex.Prefix . getOp) <$>) <$> uOpMap)

parseExpr :: AParser Int
parseExpr = Tok.whiteSpace lexer *> expr <* eof

expandArith :: String -> Shell String
expandArith s = do
  res <- runParserT parseExpr () "arithmetic expansion" s
  case res of Right s -> return . show $ s
              Left e  -> throwE . ExpErr $ s ++ ": syntax error: " ++ (show e)
