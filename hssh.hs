--import Lexer
import Exec
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import System.Exit
import System.Posix.Process
main :: IO ()
main = getLine >>= (\cmd -> evalStateT (launchCmdSub cmd) getDefaultShellEnv) >>= print >> main-- >> exitImmediately ExitSuccess
-- main = print $ parse lexer "stdin" "check if \n newline is \" accureately #\n rep#resented \n\""
