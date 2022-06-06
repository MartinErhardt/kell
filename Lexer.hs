module Lexer
( lexer
) where
import Text.Parsec
-- TODO No proper wchar support
import Text.Parsec.String (Parser)
import Data.Stack 
data Token = Word String
  -- operators as in https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#:~:text=command%20is%20parsed.-,2.10.2,-Shell%20Grammar%20Rules
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
  -- other control operators as in https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_113
  | Ampersand
  | LBracket
  | RBracket
  | SEMI
  | PIPE
  | NEWLINE
  | EOF
  deriving (Show, Eq)

parseReservedOp :: Parser [Token]
parseReservedOp = foldl1 (<|>) ((\(a,b)-> try $ string a >> ( return $ [b]) ) <$> reservedOps) 
  where reservedOps=[("&&",    AND_IF)
                    ,("||",    OR_IF)
                    ,(";;",    DSEMI)
                    ,("<<",    DLESS)
                    ,(">>",    DGREAT)
                    ,("<&",    LESSAND)
                    ,(">&",    GREATAND)
                    ,("<>",    LESSGREAT)
                    ,("<<-",   DLESSDASH)
                    ,(">|",    CLOBBER)
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
                    ,("for",   For)
                    ,("(",     LBracket)
                    ,(")",     RBracket)
                    ,(";",     SEMI)
                    ,("|",     PIPE)]

 
comment :: Parser [Token] -> Parser [Token] -> Parser [Token]
comment callback eofA = eofA <|> (char '\n' >> callback) <|> (anyChar >> (comment callback eofA))

appendStr :: String -> [Token] -> Parser [Token]
appendStr s ((Word r):o) = return $ [Word $ s ++ r] ++ o

escape :: Parser [Token] -> Parser [Token]
escape callback = (anyChar >>= (\c2 -> callback >>= (\((Word r):o) -> return $ [Word $ ['\\',c2] ++ r] ++ o ) ) )

quote :: Parser [Token] -> Parser [Token]
quote endCondition = let eofA = (eof >> unexpected("mising quote end") ) in eofA <|> endCondition
            <|> (char '\\' >> (eofA <|> escape (quote endCondition) ) )
            <|> (char '#'  >> comment (quote endCondition >>= appendStr "\n") eofA ) 
            <|> (anyChar   >>= (\c -> (quote endCondition) >>= appendStr [c] ) )


wordExpansion :: Stack String -> Parser [Token]
wordExpansion s = (foldl1 (<|>) (stackHandler <$> stackAction s)) -- Pattern matching will fail if string is empty
  where closingAction s c = if stackIsEmpty s || ( (stackPeek s) /= (Just c)) then Nothing else (\(Just s)-> Just $ fst s) $ stackPop s
        stackAction s = [("$((", Just $ stackPush s "))")
                        ,("$(",  Just $ stackPush s ")")
                        ,("${",  Just $ stackPush s "}")
                        ,("))",  closingAction s "))")
                        ,(")",   closingAction s ")")
                        ,("}",   closingAction s "}")]
        stackHandler (str, (Just a)) =try $ string str >> (if stackIsEmpty a then parseWord else quote (wordExpansion a) ) >>= appendStr str
        stackHandler (str, Nothing) = unexpected("unexpected " ++ str)

appendTokens toA =( (++) <$> return toA <*> lexer)
delimit :: [Token] -> Parser [Token]
delimit delimiters = appendTokens $ [Word ""] ++ delimiters
                
parseWord :: Parser [Token]
parseWord = let eofA = (eof >> return [Word ""]) in eofA
            <|> (parseReservedOp >>= (\opl -> delimit opl ) )
            <|> (char ' '  >> delimit []) -- parse delimiter NOTE: delimiter will be removed later 
            <|> (char '\n' >> delimit [NEWLINE] )
            <|> (char '\\' >> (eofA <|> escape parseWord) )                                    -- parse quotes
            <|> (char '\'' >> quote (char '\''>> (parseWord >>= appendStr "'" ) ) >>= appendStr "'") 
            <|> (char '"'  >> quote (char '"' >> (parseWord >>= appendStr "\"") ) >>= appendStr "\""  )
            <|> wordExpansion stackNew                                                         -- word expansion
            <|> (char '#'  >> comment (delimit []) eofA )                                        -- parse comment
            <|> (anyChar   >>= (\c-> parseWord >>= appendStr [c] ) )                           -- parse letter

lexer :: Parser [Token]
lexer = (eof>> return [EOF]) 
        <|> (parseReservedOp >>= appendTokens)
        <|> (char '\n' >> appendTokens [NEWLINE])
        <|> (char ' ' >> lexer) <|> parseWord

