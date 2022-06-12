module Exec
(
 exec,
 launchCmdSub,
 getDefaultShellEnv
) where
import Text.Parsec
import Text.Parsec.String
import Lexer
import qualified Data.Map as Map
import Data.Stack
import qualified Data.List as L
import Control.Monad.Trans.State.Lazy
import System.Posix.Process
import System.Posix.IO
import System.IO
import System.Exit
import Control.Monad.Trans.Class
import System.Posix.Signals
import System.Environment

data ShellEnv = ShellEnv { var :: Map.Map String String
                                -- , func :: Map.Map String String
                                 } deriving(Eq, Show)
type Shell = StateT ShellEnv IO 
exec :: String -> Shell (Maybe ProcessStatus)
exec cmd = expandWord cmd >>= (\fs -> launchCmd (head fs) (tail fs) (return ()) ) -- fold all commandwords and do assignments prior to that
--exec cmd = let tokens = parse lexer "stdin" cmd in lift $ print tokens

getDefaultShellEnv :: ShellEnv
getDefaultShellEnv = ShellEnv $ Map.fromList [("var1","hello guys"),("var2","echo \"$var1\""), ("var3", "/home/martin/HestonExotics/hexo -t rng")]

launchCmd :: FilePath -> [String] -> IO () -> Shell (Maybe ProcessStatus)
launchCmd cmd args redirects = do
  forkedPId <- lift . forkProcess $ do
    redirects
    getEnvironment >>= (\env -> executeFile cmd True args (Just env) )
  lift $ getProcessStatus False False forkedPId

launchCmdSub :: String -> Shell String
launchCmdSub cmd = do
  pipe      <- lift createPipe
  curEnv    <- get
  forkedPId <- lift . forkProcess $ do
    (fdToHandle . fst $ pipe) >>= hClose
    dupTo (snd pipe) stdOutput
    evalStateT (exec cmd) curEnv
    return ()
   -- exitImmediately ExitSuccess
  lift $ (fdToHandle . snd $ pipe) >>= hClose
  lift $ getProcessStatus False False forkedPId -- TODO Error handling
  lift $ ( (fdToHandle . fst $ pipe) >>=  hGetContents)
  -- lift $ signalProcess killProcess forkedPId

type Field = String
getVar :: String -> Shell (Maybe String)
getVar name = get >>= (\s -> return $ Map.lookup name (var s))

expandParams :: String -> Shell String
expandParams name = do
  var <- getVar name
  return $ case var of (Just val) -> val -- set and not null
                       (Nothing)        -> " \t\n"  -- unset

fieldExpand :: String -> Shell [Field]
fieldExpand s = do
  ifs <- expandParams "IFS"
  lift $ print ifs
  res <- (flip parse2Shell) s $ split2F ifs
  lift $ print res
  return res
--fieldExpand s = expandParams "IFS" >>= (flip parse2Shell) s . split2F 

split2F :: String -> Parser [Field]
split2F ifs = (eof >> return [""]) <|> ( choices <$> anyChar <*> (split2F ifs) )
  where choices :: Char -> [Field] -> [Field]
        choices c rest = if c `elem` ifs then 
                             if length (head rest) == 0 then
                               if length rest == 1 then [""] ++ rest 
                               else rest
                             else [""] ++ rest
                         else [ [c] ++ head rest] ++ tail rest

getVarExp :: [String] -> Parser String
getVarExp names = foldl1 (<|>) ((\a -> try $ string a >> return a ) <$> names )

getCmdSubExp :: Parser String
getCmdSubExp = char '(' >> (quote $ getDollarExp (\_ -> "") (stackPush stackNew ")") )

getParamExp :: Parser String
getParamExp = char '{'  >> (quote $ getDollarExp (\_ -> "") (stackPush stackNew "}") )

escapeUnorigQuotes :: Parser String
escapeUnorigQuotes = (eof >> return "") <|> do 
  c <- anyChar
  escapeUnorigQuotes >>= ( return . ( (if c `elem` "\xfe\xff\"'\\" then ['\xfe',c] else [c]) ++)  )

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
  where escapes = (++) <$> (escape '\xfe' <|> (char '\\' >> anyChar >>= return . (:[])) ) -- remove backslashs, but do not remove \xfe (used to escape \xff)
        dQuoteRecCond = escapes <|> ((++) <$> getDollarExp id stackNew)

parse2ShellD :: Parser (Shell [Field]) -> String -> Shell [Field]
parse2ShellD parser s = do
  case res of (Right v) -> v
              (Left e)  -> lift $ print e >> return [""]
  where res = parse parser "stdin" s

parse2Shell :: (Show a) => Parser a -> String -> Shell a
parse2Shell parser s = let res = parse parser "string" s in do
--  lift $ print res
  (return . (\(Right v) -> v)) res

parseExps :: Bool -> [String] -> Parser (Shell [Field])
parseExps doFExps names = (eof >> (return . return) [""])
        <|> (char '$'  >> expDetected fExpAction            >>= processExp ) -- TODO quoted expansions
        <|> (char '\'' >> quote  (char '\'' >> return "'" ) >>= appendStr2LF  . ("'" ++ ) )
        <|> (char '"'  >> dQuote (char '"'  >> return "")   >>= processDQuote )
        <|> (escape '\\'                                    >>= appendStr2LF )
        <|> (anyChar                                        >>= appendStr2LF . (:[]) )
  where expDetected exp = (getVarExp names >>= return . (\s -> return s >>= expandParams  >>= parse2Shell escapeUnorigQuotes >>= exp) )
                      <|> (getParamExp     >>= return . (\s -> return s >>= expandParams  >>= parse2Shell escapeUnorigQuotes >>= exp) )
                      <|> (getCmdSubExp    >>= return . (\s -> return s >>= launchCmdSub  >>= parse2Shell escapeUnorigQuotes >>= exp) )
        
        ppJoinEnds l1 l2 = init l1 ++ [last l1 ++ head l2] ++ tail l2
        appl1stOrd appl shAction1 shAction2 = appl <$> shAction1 <*> shAction2
        
        appendStr2LF str  = (\action -> ppJoinEnds [str] <$> action)          <$> (parseExps doFExps names)
        processExp action = (appl1stOrd ppJoinEnds action)                    <$> (parseExps doFExps names)
        processDQuote str = processExp $ ((:[]) . (++"\"") . ("\""++) . head) <$> (parse2ShellD (parseExps False names) str )
        
        fExpAction = if doFExps then fieldExpand else return . (:[])

expandWord :: String -> Shell [Field]
expandWord cmdWord = do
  env <- get
  fs <- let names = (fst <$> (Map.toList $ var env) ) in parse2ShellD (parseExps True names) cmdWord
-- TODO expandedPNFields <- Pathname expansion
-- how to exploit my shell? on command substitution print to stdout ['\0','\0', 'f'] -> profit
-- expand split2F to accept [String] aa field seperator
--  return fs
  --lift $ print fs
  noQuotes <- parse2Shell removeQuotes ( concat $ L.intersperse ['\xff'] fs)
--  lift $ putStrLn noQuotes
  res <- parse2Shell removeEscReSplit noQuotes-- TODO expandedPNFields <- Pathname expansion
  lift $ print res
  return res
--expandWord String -> Shell [Field]
--expandWord word = foldl1 (\a b -> ppJoinEnds <$> a <*> b ) (parseRes parseExps word)
--  where ppJoinEnds a b = init a ++ [last a ++ head b] ++ tail b
