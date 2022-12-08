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
  Cmd(..), FuncDef(..), CmpCmd(..), SmpCmd(..), Pipeline(..), AndOrList(..), SepList(..), IfClause(..), WhileLoop(..),
  Redirect(Redirect),
  Shell,
  getVar,
  putVar,
  getFunc,
  putFunc,
  quote,
  escape,
  quoteEsc,
  dQuote,
  getDollarExp,
  getErrExitCode,
  parseXBDName
)
where

import Data.Functor ((<&>))
import Data.Map as Map
import Data.Stack
import Text.Parsec
import qualified Text.Read as Rd
import Text.Parsec.String (Parser)

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy
import System.Posix.Files
import System.Posix.Types(Fd(..),FileMode)
import System.Environment
import System.Exit


data Redirect = Redirect Token Fd String deriving (Eq,Show)
data SmpCmd = SmpCmd { redirects ::  [Redirect]
                     , assign :: [(String,String)]
                     , cmdWords :: [String]
                     } deriving(Eq, Show)
data IfClause = IfClause { clauses :: [(SepList, SepList)]
                         , else_part :: Maybe SepList
                         } deriving(Eq,Show)
data WhileLoop = WhileLoop SepList SepList deriving(Eq,Show)
data FuncDef = FuncDef String Cmd deriving(Eq,Show)
data Cmd      = SCmd SmpCmd | CCmd CmpCmd [Redirect] | FCmd FuncDef deriving(Eq, Show)
--data Cmd    = Cmd SmpCmd | CmpCmd | (CmpCmd, [Redirect]) | FuncDef
data CmpCmd   = BrGroup SepList | IfCmp IfClause | WhlCmp WhileLoop deriving(Eq,Show)
--data CmpCmd = CmpCmd BraceGroup | SubShell | For_Clause | Case_Clause | If_Clause

type Pipeline  = [Cmd]
type AndOrList = [(Pipeline, Token)] -- Last Token has no meaning
type SepList   = [(AndOrList,Token)]

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
quoteEsc recCondition endCondition = endCondition <|> (eof >> unexpected "eof")
                                <|> (recCondition              <*> quoteEsc recCondition endCondition )
                                <|> ( (++) . (:[]) <$> anyChar <*> quoteEsc recCondition endCondition )

quote :: Parser String -> Parser String
quote = quoteEsc $ (++) <$> escape '\\' -- TODO catch and specify parse error with <?>

dQuote :: Parser String -> Parser String
dQuote = quoteEsc $ (++) <$> (escape '\\' <|> getDollarExp id stackNew )

getDollarExp :: (String -> String) -> Stack String -> Parser String
getDollarExp f s = foldl1 (<|>) (stackHandler <$> stackAction s) -- Pattern matching will fail if string is empty
  where closingAction s c = if stackIsEmpty s || ( stackPeek s /= Just c) then Nothing
                            else (\(Just s)-> Just $ fst s) $ stackPop s
        stackAction s = [("((", Just $ stackPush s "))")
                        ,("(",  Just $ stackPush s ")")
                        ,("{",  Just $ stackPush s "}")
                        ,("))",  closingAction s "))")
                        ,(")",   closingAction s ")")
                        ,("}",   closingAction s "}")]
        stackHandler (str, Just a) = try (string str) >> if stackIsEmpty a then return $ f str
                                                             else quote (getDollarExp f a) <&> (str ++)
        stackHandler (str, Nothing) = fail $ "no" ++ str

parseXBDName :: Parser String
parseXBDName = (++) . (:[]) <$> (letter <|> char '_') <*> many (alphaNum <|> char '_')

data ShellEnv = ShellEnv { var :: Map.Map String (String,Bool)
                         , posArgs :: Stack [String]
                         , func :: Map.Map String Cmd
                         , interactive :: Bool
                         , shFMode :: FileMode
                         } deriving(Show)

data ShellError = SyntaxErr      String
                | SBIErr
                | UtilErr        ExitCode
                | RedirSBIErr    String
                | RedirCmpCmdErr String
                | RedirFuncErr   String
                | RedirUErr      String
                | AssignErr      String
                | ExpErr         String
                | CmdNotFoundErr String ExitCode
                  deriving(Show)

getErrExitCode :: ShellError -> ExitCode
getErrExitCode e = case e of SyntaxErr _         -> ExitFailure 117
                             SBIErr              -> ExitFailure 118
                             UtilErr e           -> e
                             RedirSBIErr _       -> ExitFailure 119
                             RedirCmpCmdErr _    -> ExitFailure 120
                             RedirFuncErr _      -> ExitFailure 121
                             RedirUErr _         -> ExitFailure 122
                             AssignErr _         -> ExitFailure 123
                             ExpErr _            -> ExitFailure 124
                             CmdNotFoundErr _ ec -> ec

type Shell = ExceptT ShellError (StateT ShellEnv IO)
-- TODO factor out common parts
getVar :: String -> Shell (Maybe String)
getVar name = do
  env <- lift get
  case Rd.readMaybe name :: Maybe Int of Just n -> return $ getPos env n --TODO exclude negative n
                                         _ -> lift get <&> getVal . Map.lookup name . var
  where getVal entry = case entry of (Just (val, _)) -> Just val
                                     _               -> Nothing
        getPos env n = case stackPeek (posArgs env) of Just args -> if length args < n then Nothing
                                                              else Just $ args !! (n-1)
                                                       _ -> Nothing
putFunc :: FuncDef -> Shell ()
putFunc (FuncDef name cmd) = lift get >>= lift . put . changeNamespace ( Map.insert name cmd . func )
  where changeNamespace modifier curEnv = curEnv {func = modifier curEnv }

getFunc :: String -> Shell (Maybe Cmd)
getFunc name = lift get <&> Map.lookup name . func

--raiseExpErr :: a -> String -> Shell a
--raiseExpErr val msg = do
--  ev <- get
--  if interacive ev then return (ExpErr msg) else return val
-- infixl 4 <*>>; (<*>>) :: Monad m => m (a -> m b) -> (m a -> m b); mamb <*>> ma = join (mamb <*> ma)
putVar :: String -> String -> Shell ()
putVar name newval = do
  oldentry <- lift get <&> Map.lookup name . var
  case oldentry of (Just (_,True)) -> liftIO $ setEnv name newval
                   _               -> return ()
  lift get >>= lift . put . changeNamespace ( Map.insert name (newEntry newval oldentry) . var)
  where changeNamespace modifier curEnv = curEnv {var = modifier curEnv }
        newEntry newval oldentry = case oldentry of (Just (_,True)) -> (newval, True)
                                                    _               -> (newval, False)
