{-# LANGUAGE DeriveGeneric #-}

module OpenAI (fetchCompletion) where

import Data.Aeson
import qualified Data.String as S8
import GHC.Generics
import Network.HTTP.Simple
  ( getResponseBody,
    httpJSON,
    parseRequest,
    setRequestBearerAuth,
    setRequestBodyJSON,
  )
import System.Environment

openApiKey = "OPENAI_API_KEY"
completionApi = "https://api.openai.com/v1/completions"
textDaVinci2 = "text-davinci-002"

data OpenAIRequest = CompletionRequest
  { prompt :: String,
    model :: String
  }
  deriving (Generic, Show)

newtype OpenAIChoice = Choice {text :: String} deriving (Generic, Show)

newtype OpenAIResponse = Completion {choices :: [OpenAIChoice]} deriving (Generic, Show)

newtype Prompt = Prompt [String]

instance ToJSON OpenAIRequest where
  toEncoding = genericToEncoding defaultOptions

instance Show Prompt where
  show (Prompt xs) = (\x -> take (length x - 2) x) $ foldr (\x acc -> acc <> x <> "\n\n") "" xs
instance FromJSON OpenAIChoice
instance FromJSON OpenAIResponse

getAPIKey :: IO String
getAPIKey = getEnv openApiKey

addUserInput :: String -> Prompt -> Prompt
addUserInput x (Prompt xs) = Prompt (x : xs)

posixPrompt :: Prompt
posixPrompt = Prompt ["Convert this text to a posix shell command:", "Example: I want to list the content of my current directory\nOutput: ls -l"]

toCommand :: OpenAIResponse -> String
toCommand (Completion {choices = []}) = "No command was found!"
toCommand (Completion {choices = (Choice {text = x} : xs)}) = dropWhile (== '\n') x

fetchCompletion :: String -> IO String
fetchCompletion x = do
  request' <- parseRequest ("POST " ++ completionApi)
  apiKey <- getAPIKey

  let userPrompt = addUserInput x posixPrompt
  let body = CompletionRequest {prompt = show userPrompt, model = textDaVinci2}
  response <- httpJSON (setRequestBodyJSON body $ setRequestBearerAuth (S8.fromString apiKey) request')

  return . toCommand . getResponseBody $ response
