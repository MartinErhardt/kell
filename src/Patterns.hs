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

module Patterns(
  expandPath
)where
import qualified Control.Exception as E
import Control.Monad
import ShCommon(ShellError(..))
import ShCommon
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Directory
import Text.Regex.TDFA

pattern2Regex :: String -> String
pattern2Regex str = case parse patternParser "pattern" str of Right regex -> regex
                                                              Left  _     -> str

classExp :: Parser String
classExp = (++) <$> string "[["                  <*> restClass
  where restClass = string "]]"
                <|> (++) . (:[]) <$> anyChar <*> restClass

bracketExp :: Parser String
bracketExp = (++) . (:[]) <$> char '[' <*> secondC
  where restBracket = string "]" <|> (++) . (:[]) <$> noneOf "]" <*> restBracket
        secondC = (++) . (:[]) <$> (char '!' >> return '^')  <*> restBracket
              <|> (++) . (:[]) <$> noneOf "[]!" <*> restBracket

patternParser :: Parser String
patternParser = (eof >> return "")
            <|> (++) <$> (char '*' >> return ".*")  <*> patternParser
            <|> (++) <$> (char '?' >> return "." )  <*> patternParser
            <|> (++) <$> (char '.' >> return "\\.") <*> patternParser
            <|> (++) <$> try classExp               <*> patternParser
            <|> (++) <$> try bracketExp             <*> patternParser
            <|> (++) <$> (:[]) <$> anyChar          <*> patternParser

-- TODO check permissions
expandPathR :: String -> String -> IO [String]
expandPathR restPath basePath = do
  case restrest of "" -> contentsFiltered
                   _  -> foldl (++) [] <$> (contentsFiltered >>= mapM (expandPathR restrest) )
  where midP   = takeWhile (/='/') restPath
        restrest  = (dropWhile (=='/') . dropWhile (/='/')) restPath
        restSlash = (takeWhile (=='/') . dropWhile (/='/')) restPath
        lookupHandler :: E.IOException -> IO [String]
        lookupHandler e = return []
        baseLookup = case basePath of "" -> "./"
                                      _  -> basePath
        lookupDir = E.catch (getDirectoryContents baseLookup) lookupHandler
        isMatch = filter ((==) <$> ( =~ (pattern2Regex midP) ) <*> id)
        contents = if midP == "" || head midP == '.' then lookupDir
                   else filter ((/='.') . head) <$> lookupDir
        contentsFiltered = fmap ( ((++ restSlash) . (basePath ++) <$>) . isMatch) contents

expandPath :: String -> IO [String]
expandPath str = expandPathR relPath (takeWhile (=='/') str) >>= handleRes
  where relPath = dropWhile (=='/') str
        handleRes paths = case paths of [] -> return [str]
                                        _  -> return paths
