module Main where

import Control.Monad (void)
import System.IO (readFile)
import qualified Data.Text as Text

import Network.Linklater
import Network.Wai.Handler.Warp (run)

import qualified Utils.Strava as Strava
import qualified Utils.Log as Log

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

formatMessage :: Text.Text -> Message
formatMessage text = SimpleMessage icon name channel text

stravaBot :: Strava.Client -> Config -> Maybe Command -> IO Text.Text
stravaBot stravaClient slackConfig (Just command) = do
  Log.line ("Received a command: " ++ show command)
  maybeActivity <- Strava.latestActivity stravaClient
  let message = case maybeActivity of
                  Just activity -> text $ Strava.formatActivity activity
                  Nothing -> text "No activities you lazy bum!"
  void $ say (formatMessage message) slackConfig
  return $ text "Handled your command!\n"
stravaBot _ _ Nothing = return $ text "Failed to parse your command...\n"

readCredentials :: String -> IO String
readCredentials path = filter (/= '\n') <$> readFile path

main :: IO ()
main = do
  slackUrl <- readCredentials "secrets/slack-webhook-url"
  stravaAccessToken <- readCredentials "secrets/strava-access-token"
  stravaClient <- Strava.client (text stravaAccessToken)
  Log.line ("Listening on port " ++ show port)
  run port (slashSimple (stravaBot stravaClient (Config $ text slackUrl)))
  where port = 8000
