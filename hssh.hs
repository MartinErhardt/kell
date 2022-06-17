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
-- main = putStr "hello" >> getLine >> return ()
main = evalStateT (printPrompt "\"$PS1\"" >> lift getLine >>= cmdPrompt ) getDefaultShellEnv >> return ()
-- unpack . replace (pack "\\\n") (pack "") . pack -- >> exitImmediately ExitSuccess

printPrompt :: String -> Shell ()
printPrompt var = lift (hFlush stdout) >> expandWord var >>= lift . putStr . head >> lift (hFlush stdout)

cmdPrompt :: String -> Shell ()
cmdPrompt curCmd = do
  --lift $ print toks
  if last curCmd == '\\' then incomplete $ init curCmd 
  else case toks of (Right v) -> case parse2AST v of (Right ast) -> runSmpCmd ast    >> continuePrompt
                                                     (Left e)    -> handleErrs e
                    (Left e) -> handleErrs e
  where parse2AST = parse parseToks "charstream"
        toks      = parse lexer "tokenstream" curCmd
        incomplete :: String -> Shell ()
        incomplete str = printPrompt "\"$PS2\""    >> lift getLine >>= (cmdPrompt . (str++))
        -- continuePrompt :: Shell (Maybe ProcessStatus)
        continuePrompt = printPrompt "\"$PS1\""  >> lift getLine >>= cmdPrompt
        handleErrs :: ParseError -> Shell ()
        handleErrs e = if "eof" `elem` (messageString <$> errorMessages e) then incomplete (curCmd ++ "\n")
                       else (lift $ print e ) >> continuePrompt
--main = getLine >>= print . parse lexer "stdin" --"stdin" "check if \n newline is \" accureately #\n rep#resented \n\""
-- . parse parseToks "tokenstream" . (\(Right w)-> w) . 
