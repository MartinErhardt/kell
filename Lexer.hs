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
module Lexer
( lexer,
  Token(..),
) where
import Text.Parsec
-- TODO No proper wchar support
import Text.Parsec.String (Parser)
import Data.Stack 
import ShCommon
reservedOps = [("&&",    AND_IF)
              ,("||",    OR_IF)
              ,(";;",    DSEMI)
              ,("<<-",   DLESSDASH)
              ,("<<",    DLESS)
              ,(">>",    DGREAT)
              ,("<&",    LESSAND)
              ,(">&",    GREATAND)
              ,("<>",    LESSGREAT)
              ,(">|",    CLOBBER)
              ,("(",     LBracket)
              ,(")",     RBracket)
              ,("&",     Ampersand)
              ,(";",     SEMI)
              ,("|",     PIPE)
              ,("<",     LESS)
              ,(">",     GREAT)
              ,("\n",    NEWLINE)]


parseReservedOp :: Parser Token
parseReservedOp = foldl1 (<|>) ((\(a,b)-> try $ string a >> ( return b) ) <$> reservedOps) 
comment = (anyChar >>= handler) <|> return ""
  where handler c = case c of '\n' -> unexpected [c]
                              _    -> (++[c]) <$> comment

parseWord :: Parser String
parseWord = (eof       >>            return "" )
        <|> (char '#'  >> comment >> return "" )
        <|> (              (++)            <$> escape '\\'                       <*> (parseWord <|> return "") )
        <|> (char '\'' >> ((++) . ("'"++)  <$> quote  (char '\'' >> return "'" ) <*> (parseWord <|> return "") ) )
        <|> (char '`'  >> ((++) . ("`"++)  <$> quote  (char '`'  >> return "`" ) <*> (parseWord <|> return "") ) )
        <|> (char '"'  >> ((++) . ("\""++) <$> dQuote (char '"'  >> return "\"") <*> (parseWord <|> return "") ) )
        <|> (char '$'  >> ((++) . ("$"++)  <$> getDollarStr                      <*> (parseWord <|> return "") ) )-- word expansion
        <|> (             ((++) . (:[]))   <$> noneOf forbidden                  <*> (parseWord <|> return "") )-- parse letter
  where forbidden = ((head . fst) <$> reservedOps) ++ " "
        getDollarStr = getDollarExp id stackNew <|> return ""

lexer :: Parser [Token]
lexer = (eof      >> return [EOF])
    <|> ( ( (++) . (:[]) )       <$> parseReservedOp <*>                         lexer)
    <|> (char '#' >> comment                                                  >> lexer)
    <|> (char ' '                                                             >> lexer)
    <|> ( ( (++) . (:[]) . Word) <$> parseWord       <*> ((eof >> return []) <|> lexer) )
