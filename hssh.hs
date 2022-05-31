import Lexer
import Text.Parsec
main :: IO ()
--main = getLine >>= print . parseExpr >> main
main = getLine >>= print . (parse lexer "stdin") >> main
