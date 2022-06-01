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

  | NEOF     -- internal use only
  deriving (Show, Eq)
 
comment :: Parser [Token] -> Parser [Token] -> Parser [Token]
comment callback eofA = eofA <|> (char '\n' >> callback) <|> (anyChar   >> (comment callback eofA))

appendChar :: Char -> [Token] -> Parser [Token]
appendChar c ((Word r):o) = return $ [Word $ [c] ++ r] ++ o

escape :: Parser [Token] -> Parser [Token]
escape callback = (anyChar >>= (\c2 -> callback >>= (\((Word r):o) -> return $ [Word $ ['\\',c2] ++ r] ++ o ) ) )

quote :: Char -> Parser [Token]
quote mark = let eofA = (eof >> unexpected("mising quote end") ) in eofA
            <|> (char '\\' >> (eofA <|> escape (quote mark) ) )
            <|> (char mark >> parseWord >>= appendChar mark )
            <|> (char '#'  >> comment (quote mark) eofA ) 
            <|> (anyChar   >>= (\c -> (quote mark) >>= appendChar c ) ) 

parseWord :: Parser [Token]
parseWord = let eofA = (eof >> return [Word "", NEOF])
                endNewline = (eof >> (return $ [Word "",NEWLINE,EOF])) <|> (return $ [Word "",NEWLINE] ) in eofA
            <|> (char ' '  >> ((eof >> (return $ [Word "",EOF])) <|> (return $ [Word ""] ) ) ) -- parse delimiter NOTE: delimiter will be removed later 
            <|> (char '\n' >> endNewline)
            <|> (char '\\' >> (eofA <|> escape parseWord) )                                    -- parse quotes
            <|> (char '\'' >> quote '\''       >>= appendChar '\''  ) 
            <|> (char '"'  >> quote '"'        >>= appendChar '"'  )
            <|> (char '#'  >> comment endNewline eofA )                                        -- parse comment
            <|> (anyChar   >>= (\c-> parseWord >>= appendChar c ) )                               -- parse letter

lexSingle :: Parser [Token]
lexSingle = foldl1 (<|>) ((\(a,b)-> try $ string a >> return [b]) <$> reservedOps)  <|> parseWord  -- not the most runtime efficient
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

removeLast = reverse . tail . reverse

lexer :: Parser [Token]
lexer = let eofFinal = (eof >> return [EOF]) in eofFinal
        <|> (char ' ' >> lexer) <|> (char '#' >> comment (lexer>>= (\l -> return $ [NEWLINE]++l)) eofFinal)
        <|> ( ( \a b -> if last a == NEOF then removeLast a else ( if last a == EOF then a else a++b ) ) <$> lexSingle <*> lexer)

