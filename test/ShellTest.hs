import Test.Framework (defaultMain, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit (assertFailure)
import System.Directory
import System.Exit (ExitCode(..))
import System.Process

main :: IO ()
main = execResult >>= defaultMain

execResult :: IO [Test]
execResult = do
  scripts <- filter (not . (flip elem) [".",".."])<$> getDirectoryContents "./test/scripts"
  print $ ("./test/scripts/"++) <$> scripts
  return $ (createTestC . ("./test/scripts/"++)) <$> scripts
  where msg = ["stdout bash unequal to stdout kell", "stderr bash unequal to stderr kell", "exitcode bash unequal to kell"]
        runTest test = do
          print $ test
          exitCode <- system $ "./test/runTest.sh " ++ test
          case exitCode of ExitSuccess -> return ()
                           ExitFailure code -> assertFailure $ "Test failed: " ++ (msg !! (code-1))
        createTestC name = testCase name (runTest name)
