import Lexer
--import ShParser
import Text.Parsec
main :: IO ()
--main = getLine >>= print . parseExpr >> main
main = getLine >>= print . (parse lexer "stdin") >> main
--main = print $ parse lexer "stdin" "check if \n newline is \" accureately #\n rep#resented \n\""
