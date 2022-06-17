--   Copyright 2022 Martin Erhardt
import Lexer
import Exec (Shell)
import Exec
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import System.Exit
import System.Posix.Process
import Text.Parsec
import Text.Parsec 
import qualified Data.Text as Txt
import TokParser
import Text.Parsec.Error(Message(..))
import Text.Parsec.Error
import System.IO

main :: IO ()
main = evalStateT (printPrompt "\"$PS1\"" >> lift getLine >>= cmdPrompt ) getDefaultShellEnv >> return ()
-- unpack . replace (pack "\\\n") (pack "") . pack -- >> exitImmediately ExitSuccess

printPrompt :: String -> Shell ()
printPrompt var = lift (hFlush stdout) >> expandWord var >>= lift . putStr . head >> lift (hFlush stdout)

cmdPrompt :: String -> Shell ()
cmdPrompt curCmd = do
  --lift $ print toks
  if curCmd == "" then continuePrompt
  else if last curCmd == '\\' then incomplete $ init curCmd 
  else case toks of (Right v) -> case parse2AST v of (Right ast) -> runSmpCmd ast >> continuePrompt
                                                     (Left e)    -> handleErrs e
                    (Left e) -> handleErrs e
  where parse2AST = parse parseToks "charstream"
        toks      = parse lexer "tokenstream" curCmd
        incomplete str = printPrompt "\"$PS2\"" >> lift getLine >>= (cmdPrompt . (str++))
        continuePrompt = printPrompt "\"$PS1\"" >> lift getLine >>= cmdPrompt
        handleErrs e = if "eof" `elem` (messageString <$> errorMessages e) then incomplete (curCmd ++ "\n")
                       else (lift $ print e ) >> continuePrompt
--main = getLine >>= print . parse lexer "stdin" --"stdin" "check if \n newline is \" accureately #\n rep#resented \n\""
-- . parse parseToks "tokenstream" . (\(Right w)-> w) . 
