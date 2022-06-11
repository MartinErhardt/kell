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

data ShellEnv = ShellEnv { var :: Map.Map String (Maybe String)
                                -- , func :: Map.Map String String
                                 } deriving(Eq, Show)
type Shell = StateT ShellEnv IO 
exec :: String -> Shell (Maybe ProcessStatus)
exec cmd = expandWord cmd >>= (\fs -> launchCmd (head fs) (tail fs) (return ()) ) -- fold all commandwords and do assignments prior to that
--exec cmd = let tokens = parse lexer "stdin" cmd in lift $ print tokens

getDefaultShellEnv :: ShellEnv
getDefaultShellEnv = ShellEnv Map.empty

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
getVar :: String -> Shell (Maybe (Maybe String))
getVar name = get >>= (\s -> return $ Map.lookup name (var s))

expandParams :: String -> Shell String
expandParams name = do
  var <- getVar name
  return $ case var of (Just(Just val)) -> val -- set and not null
                       (Just Nothing)   -> ""  -- null
                       (Nothing)        -> ""  -- unset

fieldExpand :: String -> Shell [Field]
fieldExpand s = expandParams "IFS" >>= (flip parse2Shell) s . split2F 

split2F :: String -> Parser [Field]
split2F ifs = (eof >> return [""]) <|> ( choices <$> anyChar <*> (split2F ifs) )
  where choices :: Char -> [Field] -> [Field]
        choices c rest = if c `elem` ifs then 
                             if length (head rest) == 0 then
                               if length rest == 0 then [""] ++ rest 
                               else rest
                             else rest
                           else [ [c] ++ head rest] ++ tail rest

getVarExp :: [String] -> Parser String
getVarExp names = foldl1 (<|>) ((\a -> try $ string a >> return a ) <$> names )

getCmdSubExp :: Parser String
getCmdSubExp = char '(' >> getDollarExp (\_ -> "") (stackPush stackNew ")")

getParamExp :: Parser String
getParamExp = char '{' >>  getDollarExp (\_ -> "") (stackPush stackNew "}")

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
           <|> (char '\''   >> (++) <$> quoteEsc ['\xfe','\\'] (char '\'' >> return "") <*> removeQuotes )
           <|> (char '"'    >> (++) <$> quoteEsc ['\xfe','\\'] (char '"'  >> return "") <*> removeQuotes )
           <|> (char '\\'   >> (++) <$> (anyChar >>= return . (:[]) )                   <*> removeQuotes )
           <|> (char '\xfe' >> (++) <$> (anyChar >>= return . (['\xfe']++) . (:[]) )    <*> removeQuotes )
           <|> (               (++) <$> (anyChar >>= return . (:[]) )                   <*> removeQuotes )

parse2ShellD :: Parser (Shell a) -> String -> Shell a
parse2ShellD parser s = (\(Right v) -> v) $ parse parser "stdin" s

parse2Shell :: (Show a) => Parser a -> String -> Shell a
parse2Shell parser s = let res = parse parser "string" s in do
--  lift $ print res
  (return . (\(Right v) -> v)) res
--  where handler parseRes = case parseRes of (Right v) -> return v
--                                            (Lefti e)    -> print e
joinFields :: Parser (Shell [Field]) -> Parser (Shell [Field]) -> Parser (Shell [Field])
joinFields parser1 parser2 = combine <$> parser1 <*> parser2
  where ppJoinEnds l1 l2 = init l1 ++ [last l1 ++ head l2] ++ tail l2
        combine fieldL1 fieldL2 = ppJoinEnds <$> fieldL1 <*> fieldL2

parseExps :: [String] -> Parser (Shell [Field])
parseExps names = (eof >> (return . return) [""])
        <|> (char '$'  >>  joinFields (expDetected fieldExpand) (parseExps names)) -- TODO quoted expansions
        <|> (char '\'' >>  quote (char '\'' >> return "'") >>= appendStr . ("'" ++ ) )
        <|> (anyChar   >>= appendStr . (:[]) ) 
  where expDetected exp = (getVarExp names >>= return . (\s -> return s >>= expandParams  >>= parse2Shell escapeUnorigQuotes >>= exp) )
                      <|> (getParamExp     >>= return . (\s -> return s >>= expandParams  >>= parse2Shell escapeUnorigQuotes >>= exp) )
                      <|> (getCmdSubExp    >>= return . (\s -> return s >>= launchCmdSub  >>= parse2Shell escapeUnorigQuotes >>= exp) )
        appendStr str = parseExps names >>= (\shAction -> return $ shAction >>=(\fs -> return $ [str ++ head fs] ++ tail fs) )


expandWord :: String -> Shell [Field]
expandWord cmdWord = do
  env <- get
  fs <- let names = (fst <$> (Map.toList $ var env) ) in parse2ShellD (parseExps names) cmdWord
-- TODO expandedPNFields <- Pathname expansion
-- how to exploit my shell? on command substitution print to stdout ['\0','\0', 'f'] -> profit
-- expand split2F to accept [String] aa field seperator
--  return fs
--  lift $ print fs
  noQuotes <- parse2Shell removeQuotes ( concat $ L.intersperse ['\xff'] fs)
--  lift $ print noQuotes
  parse2Shell removeEscReSplit noQuotes-- TODO expandedPNFields <- Pathname expansion
--expandWord String -> Shell [Field]
--expandWord word = foldl1 (\a b -> ppJoinEnds <$> a <*> b ) (parseRes parseExps word)
--  where ppJoinEnds a b = init a ++ [last a ++ head b] ++ tail b
