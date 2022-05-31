module Lexer
( lexer
) where
import Text.Parsec
-- TODO No proper wchar support
import Text.Parsec.String (Parser)
--import Text.ParserCombinators.Parsec.Char
--import Text.ParserCombinators.Parsec.Prim


-- For info see
-- https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#:~:text=command%20is%20parsed.-,2.10.2,-Shell%20Grammar%20Rules
data Token = Word String
  | AND_IF   -- &&
  | OR_IF    -- ||
  | DSEMI    -- ;;
  | DLESS    -- <<
  | DGREAT   -- >>
  | LESSAND  -- <&
  | GREATAND -- >&
  | LESSGREAT-- <>
  | DLESSDASH-- <<-
  | CLOBBER  -- >|
  | If       -- if
  | Then     -- then
  | Else     -- else
  | Elif     -- elif
  | Fi       -- fi
  | Do       -- do
  | Done     -- done
  | Case     -- case
  | Esac     -- esac
  | While    -- while
  | Until    -- until
  | For      -- for
  | NEWLINE
  | EOF
  deriving (Show, Eq)
-- escape :: String -> Parser [Token]
-- escape = do 
-- 	c <- char
-- 	return [Word (c:body)]
--eofWordEnd :: String -> CharParser st Token
eofWordEnd word = do
  eof
  if word == "" then unexpected("unexpected EOF")
                else return $ Word word

eofSquote=(eof >> unexpected("unexpected eof while looking for '"))
quote body = eofSquote <|> do
  c <- anyChar 
  case c of '\'' -> parseWord body
            _ -> quote $ body++[c]

--dquote :: String -> CharParser st Token
dquote body = eofDquote <|> do
  c <- anyChar
  case c of '"' -> parseWord body -- return [Word body]
            '\\' -> eofDquote <|> do{
              c2 <- anyChar;
              if c2 `elem` ['`', '"',   '\'',   '\n'] then dquote $ body++[c2] else dquote $ body++[c,c2]
            }
            _ -> dquote $ body++[c]
  where eofDquote =(eof >> unexpected("unexpected eof while looking for \"")) --quote :: String -> CharParser st Token

--parseWord :: String -> CharParser st Token
parseWord word = eofWordEnd word <|> do
  c <- anyChar
  case c of '\n' -> return $ Word $ word++[c] -- Hack: will remove newline later and add as seperate token
            ' ' -> if word == "" then unexpected("expected word") else return $ Word $ word++[c]
            '"' -> dquote $ word++[c]
            '\'' -> quote $ word++[c]
            '\\' -> do{
              c2 <- anyChar;
              parseWord $ word++[c,c2]
            }
            _ -> parseWord $ word++[c]

lexSingle :: Parser Token
lexSingle = foldl1 (<|>) ((\(a,b)-> try $ string a >> return b) <$> reservedOps)  <|> parseWord "" -- not the most runtime efficient, but proud on neat functional pattern
  where reservedOps=[("&&",AND_IF)
                    ,("||", OR_IF)
                    ,(";;", DSEMI)
                    ,("<<", DLESS)
                    ,(">>", DGREAT)
                    ,("<&", LESSAND)
                    ,(">&", GREATAND)
                    ,("<>", LESSGREAT)
                    ,("<<-", DLESSDASH)
                    ,(">|", CLOBBER)
                    ,("if", If)
                    ,("then", Then)
                    ,("else", Else)
                    ,("elif", Elif)
                    ,("fi", Fi)
                    ,("done", Done)
                    ,("do", Do)
                    ,("case", Case)
                    ,("esac", Esac)
                    ,("while", While)
                    ,("until", Until)
                    ,("for", For)]

removeLast = reverse . tail . reverse -- slow ass

deEOF :: Token -> [Token]
deEOF (Word w) = case last w of '\n' -> [(Word $ removeLast w),EOF]
                                ' '  -> [(Word $ removeLast w),EOF]
                                _ ->    [(Word w)]
deEOF t=[t]


deDelimit :: Token -> [Token]
deDelimit (Word w) = case last w of '\n' -> [(Word $ removeLast w)]
                                    ' '  -> [(Word $ removeLast w)]
                                    _ ->    [(Word w)]
deDelimit t=[t]

lexer :: Parser [Token]
lexer = (char ' ' >> lexer) <|> (eof >> return [EOF]) <|> do
  t <- lexSingle
  (eof>> (return $ deEOF t) ) <|> (lexer>>= (\l -> return $ (deDelimit t++l)) )
--
--  word <- lexSingle
--  try(eof >> (return $ words++[EOF]) ) <|> (return words)

