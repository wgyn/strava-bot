module Main where

import Control.Monad (void)
import System.IO (readFile)
import qualified Data.Text

import Network.Linklater

-- Convenience method, since Linklater deals with Text
text :: String -> Data.Text.Text
text str = Data.Text.pack str

main :: IO ()
main = do
  slackUrl <- filter (/= '\n') <$> readFile "secrets/slack-webhook-url"
  putStrLn ("+ Listening on port " ++ show port)
  void $ say message (Config $ text slackUrl)
  where
    port = 8000
    icon = EmojiIcon $ text "hand"
    botName = text "strava-bot"
    channel = GroupChannel $ text "general"
    messageContents = text "Hello, world!"
    message = SimpleMessage icon botName channel messageContents
