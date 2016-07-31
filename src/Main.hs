module Main where

import Control.Monad (void)
import System.IO (readFile)
import qualified Data.Text as Text

import Data.Time.Format
import Data.Time.LocalTime

import Network.Linklater
import Network.Wai.Handler.Warp (run)

-- Convenience method, since Linklater deals with Text
text :: String -> Text.Text
text str = Text.pack str

-- Begin bot configuration
icon :: Icon
icon = EmojiIcon $ text "hand"

name :: Text.Text
name = text "strava-bot"

channel :: Channel
channel = GroupChannel $ text "general"

-- End bot configuration

-- Begin logging utils

defaultFormat :: String
defaultFormat = "%F %T %Z"

-- TODO: Get a real logging framework
logLine :: String -> IO ()
logLine s = do
  zonedTime <- getZonedTime
  let timestamp = formatTime defaultTimeLocale defaultFormat zonedTime
  putStrLn ("[" ++ timestamp ++ "] " ++ s)

-- End logging utils

formatMessage :: Text.Text -> Message
formatMessage text = SimpleMessage icon name channel text

stravaBot :: Config -> Maybe Command -> IO Text.Text
stravaBot config (Just command) = do
  logLine ("Received a command: " ++ show command)
  void $ say (formatMessage $ _commandName command) config
  return $ text "Handled a command!\n"
stravaBot _ Nothing = return $ text "Failed to parse incoming command...\n"

main :: IO ()
main = do
  slackUrl <- filter (/= '\n') <$> readFile "secrets/slack-webhook-url"
  logLine ("Listening on port " ++ show port)
  run port (slashSimple (stravaBot $ Config $ text slackUrl))
  where port = 8000
