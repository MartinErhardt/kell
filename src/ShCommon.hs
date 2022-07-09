--   Copyright 2022 Martin Erhardt
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--       http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

module ShCommon(
  ShellEnv(..),
  Token(..),
  ShellError(..),
  Shell,
  getVar,
  putVar,
  quote,
  escape,
  quoteEsc,
  dQuote,
  getDollarExp,
  parseXBDName
)
where
import Data.Stack
import Data.Map as Map
import Text.Parsec
import Text.Parsec.String (Parser)

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import System.Posix.Types(Fd(..),FileMode)
import System.Posix.Files
import System.Environment

data Token = Word String
  -- operators as in https://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html#:~:text=command%20is%20parsed.-,2.10.2,-Shell%20Grammar%20Rules
  | AND_IF   -- &&
  | OR_IF    -- ||
  | DSEMI    -- ;;
  | DLESSDASH-- <<-
  | DLESS    -- <<
  | DGREAT   -- >>
  | LESSAND  -- <&
  | GREATAND -- >&
  | LESSGREAT-- <>
  | CLOBBER  -- >|
  | If       -- if
  | Then     -- then
  | Else     -- else
  | Elif     -- elif
  | Fi       -- fi
  | Do       -- do
  | Done     -- done
  | Case     -- case
  | Esac     -- esac
  | While    -- while
  | Until    -- until
  | For      -- for
  -- other control operators as in https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_113
  | Ampersand
  | LBracket
  | RBracket
  | SEMI
  | PIPE
  | NEWLINE
  | EOF
  -- not sure if conforming
  | LESS
  | GREAT
  -- reserved words
  | Lbrace   -- {
  | Rbrace   -- }
  | Bang     -- !
  | In       -- in
  deriving (Ord, Show, Eq)

escape :: Char -> Parser String
escape identifier = char identifier >> ([identifier]++) .(:[]) <$> anyChar

quoteEsc :: Parser (String -> String) -> Parser String -> Parser String
quoteEsc recCondition endCondition = endCondition <|> (eof >> unexpected("eof") )
                                <|> (recCondition              <*> (quoteEsc recCondition endCondition) )
                                <|> ( (++) . (:[]) <$> anyChar <*> (quoteEsc recCondition endCondition) )

quote :: Parser String -> Parser String
quote = quoteEsc $ (++) <$> escape '\\' -- TODO catch and specify parse error with <?>

dQuote :: Parser String -> Parser String
dQuote = quoteEsc $ (++) <$> (escape '\\' <|> (getDollarExp id stackNew ) )

getDollarExp :: (String -> String) -> Stack String -> Parser String
getDollarExp f s = (foldl1 (<|>) (stackHandler <$> stackAction s)) -- Pattern matching will fail if string is empty
  where closingAction s c = if stackIsEmpty s || ( (stackPeek s) /= (Just c)) then Nothing
                            else (\(Just s)-> Just $ fst s) $ stackPop s
        stackAction s = [("((", Just $ stackPush s "))")
                        ,("(",  Just $ stackPush s ")")
                        ,("{",  Just $ stackPush s "}")
                        ,("))",  closingAction s "))")
                        ,(")",   closingAction s ")")
                        ,("}",   closingAction s "}")]
        stackHandler (str, (Just a)) = (try $ string str) >> if stackIsEmpty a then return $ f str
                                                             else quote (getDollarExp f a) >>= return . (str++)
        stackHandler (str, Nothing) = fail $ "no" ++ str

parseXBDName :: Parser String
parseXBDName = (++) . (:[]) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')

data ShellEnv = ShellEnv { var :: Map.Map String (String,Bool)
                      -- , func :: Map.Map String String
                         , interactive :: Bool
                         , shFMode :: FileMode
                         } deriving(Eq, Show)
data ShellError = SyntaxErr      String
                | SBIErr
                | UtilErr
                | RedirSBIErr    String
                | RedirCmpCmdErr String
                | RedirFuncErr   String
                | RedirUErr      String
                | AssignErr      String
                | ExpErr         String
                | CmdNotFoundErr String
                  deriving(Show)

type Shell = StateT ShellEnv (EitherT ShellError IO)

getVar :: String -> Shell (Maybe String)
getVar name = get >>= return . getVal . (Map.lookup name) . var
  where getVal entry = case entry of (Just (val, _)) -> Just val
                                     _               -> Nothing

--raiseExpErr :: a -> String -> Shell a
--raiseExpErr val msg = do
--  ev <- get
--  if interacive ev then return (ExpErr msg) else return val
-- infixl 4 <*>>; (<*>>) :: Monad m => m (a -> m b) -> (m a -> m b); mamb <*>> ma = join (mamb <*> ma)

putVar :: String -> String -> Shell ()
putVar name newval = do
  oldentry <- get >>= return . (Map.lookup name) . var
  case oldentry of (Just (_,True)) -> liftIO $ setEnv name newval
                   _               -> return ()
  get >>= put . changeNamespace ( (Map.insert name (newEntry newval oldentry)) . var)
  where changeNamespace modifier curEnv = curEnv {var = modifier curEnv }
        newEntry newval oldentry = case oldentry of (Just (_,True)) -> (newval, True)
                                                    _               -> (newval, False)

