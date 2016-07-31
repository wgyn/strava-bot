module Main where

import Control.Monad (void)
import System.IO (readFile)
import qualified Data.Text as Text

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

formatMessage :: Text.Text -> Message
formatMessage text = SimpleMessage icon name channel text

stravaBot :: Config -> Maybe Command -> IO Text.Text
stravaBot config (Just command) = do
  void $ say (formatMessage $ _commandName command) config
  return $ text "Handled a command!\n"
stravaBot config Nothing = return $ text "Failed to parse an incoming command...\n"

main :: IO ()
main = do
  slackUrl <- filter (/= '\n') <$> readFile "secrets/slack-webhook-url"
  putStrLn ("+ Listening on port " ++ show port)
  run port (slashSimple (stravaBot $ Config $ text slackUrl))
  where port = 8000
