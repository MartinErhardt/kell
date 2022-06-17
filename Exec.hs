--   Copyright 2022 Martin Erhardt
module Exec
(
 runSmpCmd,
 getDefaultShellEnv,
 expandWord,
 Shell,
 getVar
) where
import Lexer
import TokParser (SmpCmd(..), parseToks, Redirect(..))

import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Data.Stack
import qualified Data.List as L
import qualified Text.Read as Rd
import Control.Monad.Trans.State.Lazy
import System.Posix.Process
import System.Posix.IO
import System.Posix.IO(OpenFileFlags(..))
import System.IO
import System.Exit
import Control.Monad.Trans.Class
import System.Posix.Signals
import System.Environment
import Data.Bits
import System.Posix.Types(Fd(..),FileMode)
import Data.Int(Int32(..))
import System.Posix.Files

data ShellEnv = ShellEnv { var :: Map.Map String String
                      -- , func :: Map.Map String String
                         , shFMode :: FileMode
                         } deriving(Eq, Show)
type Shell = StateT ShellEnv IO

runSmpCmd :: SmpCmd -> Shell (Maybe ProcessStatus)
runSmpCmd cmd = if cmdWords cmd /= [] then do
                  allFields <- foldl1 (\a b -> (++) <$> a <*> b)  (expandWord <$> cmdWords cmd)
                  if allFields /= [] then launchCmd (head allFields) (tail allFields) (execAssigns >> execRedirects)
                  else execLocal
                else execLocal
  where execAssigns =   if assign cmd /= []    then foldl1 (>>) (doAssign   <$> (assign cmd))    else return ()
        execRedirects = if redirects cmd /= [] then foldl1 (>>) (doRedirect <$> (redirects cmd)) else return ()
        execLocal = execAssigns >> execRedirects >> (return . Just $ Exited ExitSuccess)

getDefaultShellEnv :: ShellEnv
getDefaultShellEnv = ShellEnv (Map.fromList [("PS1","$ "), ("PS2","> ")]) ownerModes

execSubShell :: String -> Shell ()
execSubShell cmd = case toks of (Right val) -> case parse2Ast val of (Right ast) -> runSmpCmd ast >> return ()
                                                                     (Left err)  -> lift $ print err
                                (Left err)  -> lift $ print err
  where toks = parse lexer "subshell" cmd
        parse2Ast = parse parseToks "tokenstreamsubshell"

launchCmd :: FilePath -> [String] -> Shell () -> Shell (Maybe ProcessStatus)
launchCmd cmd args prepare = do 
  curEnv <- get
  forkedPId <- lift . forkProcess $ evalStateT (prepare >> lift runInCurEnv) curEnv >> return ()
  lift $ getProcessStatus True True forkedPId
  where runInCurEnv = getEnvironment >>= (executeFile cmd True args) . Just

launchCmdSub :: String -> Shell String
launchCmdSub cmd = do
  pipe      <- lift createPipe
  curEnv    <- get
  forkedPId <- lift . forkProcess $ do
    (fdToHandle . fst $ pipe) >>= hClose
    dupTo (snd pipe) stdOutput
    evalStateT (execSubShell cmd) curEnv
    return ()
  lift $ (fdToHandle . snd $ pipe) >>= hClose
  lift $ getProcessStatus False False forkedPId -- TODO Error handling
  lift $ ( (fdToHandle . fst $ pipe) >>= hGetContents >>= return . reverse . dropWhile (=='\n') . reverse)

doAssign :: (String,String) -> Shell ()
doAssign (name,word) = expandNoSplit word >>= putVar name

doRedirect :: Redirect -> Shell ()
doRedirect (Redirect tok fd path) = get >>= lift . (getAction tok (Fd $ fromIntegral fd) path) . shFMode >>= return . return ()
  where truncOFlag  = (OpenFileFlags False False False False True)
        appendOFlag = (OpenFileFlags True  False False False False)
        redirAction = [(LESS,      (\fd path m -> openFd path ReadOnly  (Just m) truncOFlag  >>= (flip dupTo $ fd)))
                      ,(GREAT,     (\fd path m -> openFd path WriteOnly (Just m) truncOFlag  >>= (flip dupTo $ fd)))
                      ,(DGREAT,    (\fd path m -> openFd path WriteOnly (Just m) appendOFlag >>= (flip dupTo $ fd)))
                      ,(LESSGREAT, (\fd path m -> openFd path ReadWrite (Just m) truncOFlag  >>= (flip dupTo $ fd)))
                      ,(LESSAND,   (\fd1 fd2 m -> dupTo fd1 (Fd . fromIntegral $ Rd.read fd2) ) )
                      ,(GREATAND,  (\fd1 fd2 m -> dupTo fd1 (Fd . fromIntegral $ Rd.read fd2) ) )]
        getAction tok = (\(Just v) -> v) $ Map.lookup tok (Map.fromList redirAction) -- Pattern matching failure for Here-Documenents

getVar :: String -> Shell (Maybe String)
getVar name =     get >>= return . (Map.lookup name) . var 

putVar :: String -> String -> Shell ()
putVar name val = get >>= return . changeNamespace ( (Map.insert name val) . var) >>= put
  where changeNamespace modifier curEnv = ShellEnv (modifier curEnv) (shFMode curEnv)
-----------------------------------------------Word Expansion--------------------------------------------------------------
type Field = String

expandParams :: String -> Shell String
expandParams name = getVar name >>= return . handleVal
  where handleVal var =  case var of (Just val) -> val -- set and not null
                                     (Nothing)  -> ""  -- unset

fieldExpand :: String -> Shell [Field]
fieldExpand s = getVar "IFS" >>= return . ifsHandler >>= (flip parse2Shell) s . split2F 
  where ifsHandler var = case var of (Just val) -> val
                                     (Nothing)  -> " \t\n"

split2F :: String -> Parser [Field]
split2F ifs = (eof >> return [""])
          <|> ( (\c fs -> [ [c] ++ head fs] ++ tail fs) <$> noneOf ifs <*> split2F ifs )
          <|> (                                  oneOf ifs >> ([""]++) <$> split2F ifs )

escapeUnorigQuotes :: Parser String
escapeUnorigQuotes = (eof >> return "") <|> handle <$> anyChar <*> escapeUnorigQuotes
  where handle c str =  (if c `elem` "\xfe\xff\"'\\" then ['\xfe',c] else [c]) ++ str

removeEscReSplit :: Parser [Field]
removeEscReSplit = (eof >> return [""])
                <|> (char '\xff' >> removeEscReSplit >>= return . ([""]++) )
                <|> (char '\xfe' >> appendC <$> anyChar <*> removeEscReSplit )
                <|> (               appendC <$> anyChar <*> removeEscReSplit )
  where appendC c rest = [[c] ++ head rest] ++ tail rest

removeQuotes :: Parser String
removeQuotes = (eof >> return "")
           <|> (char '\''   >> (++) <$> quoteEsc escapes       (char '\'' >> return "") <*> removeQuotes )
           <|> (char '"'    >> (++) <$> quoteEsc dQuoteRecCond (char '"'  >> return "") <*> removeQuotes )
           <|> (char '\\'   >> (++) <$> (anyChar >>= return . (:[]) )                   <*> removeQuotes )
           <|> (char '\xfe' >> (++) <$> (anyChar >>= return . (['\xfe']++) . (:[]) )    <*> removeQuotes )
           <|> (               (++) <$> (anyChar >>= return . (:[]) )                   <*> removeQuotes )
  where escapes = (++) <$> (escape '\xfe' <|> (char '\\' >> anyChar >>= return . (:[])) ) 
        dQuoteRecCond = escapes <|> ((++) <$> getDollarExp id stackNew)

parse2ShellD :: Parser (Shell [Field]) -> String -> Shell [Field]
parse2ShellD parser s = case parse parser "string" s of (Right v) -> v
                                                        (Left e)  -> lift $ print e >> return [""]

parse2Shell :: (Show a) => Parser a -> String -> Shell a
parse2Shell parser = return . (\(Right v) -> v) . parse parser "string"

parseExps :: Bool -> [String] -> Parser (Shell [Field])
parseExps doFExps names = (eof >> (return . return) [""])
        <|> (char '$'  >> dExpDetected                     >>= processFExp ) -- TODO quoted expansions
        <|> (char '\'' >> quote  (char '\'' >> return "" ) >>= appendStr2LF  . enclose '\'') 
        <|> (char '"'  >> dQuote (char '"'  >> return "" ) >>= processDQuote )
        <|> (char '`'  >> bQuote (char '`'  >> return "" ) >>= processBQuote )
        <|> (escape '\\'                                   >>= appendStr2LF )
        <|> (anyChar                                       >>= appendStr2LF . (:[]) )
  where ppJoinEnds l1 l2 = init l1 ++ [last l1 ++ head l2] ++ tail l2
        enclose c = (++[c]) . ([c]++)
        appl1stOrd appl shAction1 shAction2 = appl <$> shAction1 <*> shAction2
        
        bQuote = quoteEsc ( (++) <$> (char '\\' >> (((char '\\' <|> char '$' <|> char '`') >> anyChar >>= return . (:[]) ) 
                                               <|> (anyChar >>= return . (['\\']++) . (:[]) ) ) ) )
        getVarExp :: [String] -> Parser String
        getVarExp names = foldl1 (<|>) ((\a -> try $ string a >> return a ) <$> names )
        getCmdSubExp = char '(' >> (quote $ getDollarExp (\_ -> "") (stackPush stackNew ")") )
        getParamExp  = char '{' >> (quote $ getDollarExp (\_ -> "") (stackPush stackNew "}") )
        
        appendStr2LF str =                      (ppJoinEnds [str] <$>) <$> (parseExps doFExps names)
        processFExp action =            (appl1stOrd ppJoinEnds action) <$> (parseExps doFExps names)
        processDQuote str = processFExp $ ((:[]) . enclose '"' . head) <$> (parse2ShellD (parseExps False names) str )
        processPost str = (parse2Shell escapeUnorigQuotes str >>= if doFExps then fieldExpand else return . (:[]) )
        processBQuote :: String -> Parser (Shell [Field])
        processBQuote s =                    return   (      return s >>= launchCmdSub >>= processPost)
        dExpDetected = (getVarExp names  >>= return . (\s -> return s >>= expandParams >>= processPost) )
                   <|> (getParamExp      >>= return . (\s -> return s >>= expandParams >>= processPost) )
                   <|> (getCmdSubExp     >>= return . (\s -> return s >>= launchCmdSub >>= processPost) )

expandNoSplit :: String -> Shell String
expandNoSplit cmdWord = parseExpsWNames >>= (flip parse2ShellD) cmdWord >>= (parse2Shell removeQuotes) . head
  where parseExpsWNames = ((fst <$>) <$> (get >>= return . Map.toList . var) ) >>= return . (parseExps False)

expandWord :: String -> Shell [Field]
expandWord cmdWord = do
  env <- get
  fs <- let names = (fst <$> (Map.toList $ var env) ) in parse2ShellD (parseExps True names) cmdWord
-- TODO expandedPNFields <- Pathname expansion
-- how to exploit my shell? on command substitution print to stdout ['\0','\0', 'f'] -> profit
-- expand split2F to accept [String] aa field seperator
--  lift $ print fs
  noQuotes <- parse2Shell removeQuotes ( concat . L.intersperse ['\xff'] $ filter (/="") fs)
--  lift $ putStrLn noQuotes
  res <- parse2Shell removeEscReSplit noQuotes-- TODO expandedPNFields <- Pathname expansion
--  lift $ print res
  return res
--expandWord String -> Shell [Field]
--expandWord word = foldl1 (\a b -> ppJoinEnds <$> a <*> b ) (parseRes parseExps word)
--  where ppJoinEnds a b = init a ++ [last a ++ head b] ++ tail b
