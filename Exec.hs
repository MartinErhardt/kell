module Exec
(
 exec,
 launchCmdSub,
 getDefaultShellEnv
) where
import Text.Parsec
import Lexer
import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy
import System.Posix.Process
import System.Posix.IO
import System.IO
import System.Exit
import Control.Monad.Trans.Class
import System.Posix.Signals
import System.Environment

data ShellEnvData = ShellEnvData { var :: Map.Map String String
                                -- , func :: Map.Map String String
                                 } deriving(Eq, Show)
type ShellEnv = StateT ShellEnvData IO 
exec :: String -> ShellEnv ()
exec cmd = let tokens = parse lexer "stdin" cmd in lift $ print tokens

getDefaultShellEnv :: ShellEnvData
getDefaultShellEnv = ShellEnvData Map.empty

launchCmd :: FilePath -> [String] -> IO () -> ShellEnv (Maybe ProcessStatus)
launchCmd cmd args redirects = do
  forkedPId <- lift . forkProcess $ do
    redirects
    getEnvironment >>= (\env -> executeFile cmd True args (Just env) )
  lift $ getProcessStatus True False forkedPId

launchCmdSub :: String -> ShellEnv String
launchCmdSub cmd = do
  pipe      <- lift createPipe
  curEnv    <- get
  forkedPId <- lift . forkProcess $ do
    (fdToHandle . fst $ pipe) >>= hClose
    dupTo (snd pipe) stdOutput
    evalStateT (exec cmd) curEnv
    -- exitImmediately ExitSuccess
  lift $ (fdToHandle . snd $ pipe) >>= hClose
  lift $ getProcessStatus False False forkedPId
  lift $ ( (fdToHandle . fst $ pipe) >>=  hGetContents)
  -- lift $ signalProcess killProcess forkedPId

