module ExpArith(
  expandArith
)where

import ShCommon

import Data.Bits
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Trans.Class

import qualified Text.Parsec.Language as Lang
import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok
import qualified Text.Read as Rd

type AParser a = ParsecT String () Shell a
--data Expr = Num Int | Var Id
--  | Ass Id BinOp Expr
--  | Op BinOp Expr Expr
--  deriving (Eq, Show)

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
  where ops = concat . fst . unzip $ (unzip <$> bOpMap)
        style = Lang.emptyDef
         {Tok.commentStart    = ""
         ,Tok.commentEnd      = ""
         ,Tok.commentLine     = ""
         ,Tok.identStart      = letter   <|> char '_'
         ,Tok.identLetter     = alphaNum <|> char '_'
         ,Tok.opStart         = Tok.opLetter style
         ,Tok.opLetter        = oneOf (concat ops)
         ,Tok.reservedOpNames = ops
         ,Tok.reservedNames   = []
         ,Tok.caseSensitive   = True
         }

numb :: AParser Int
numb      = Tok.natural lexer >>= return . fromIntegral

getVarVal :: AParser Int
getVarVal = Tok.identifier lexer >>= (lift . getVar) >>= handleVar
  where handleVar v = case v of (Just v) -> case Rd.readMaybe v of (Just n) -> return n
                                                                   _        -> unexpected("no integer")
                                _        -> unexpected("var does not exist")

baseExpr :: AParser Int
baseExpr = Tok.parens lexer expr <|> getVarVal <|> numb

expr :: AParser Int
expr = Ex.buildExpressionParser table baseExpr
  where infixOp x f = Ex.Infix (Tok.reservedOp lexer x >> return f) Ex.AssocLeft
        table :: [[Ex.Operator String () Shell Int]]
        table = ((uncurry infixOp) <$>) <$> bOpMap

parseExpr :: AParser Int
parseExpr = do
  Tok.whiteSpace lexer
  r <- expr
  eof
  return r

expandArith :: String -> Shell String
expandArith s = do
  res <- runParserT parseExpr () "arith" s
  case res of (Right s) -> return . show $ s
              _         -> return "" -- TODO throw error
