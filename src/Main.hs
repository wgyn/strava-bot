module Main where

import Control.Monad (void)
import System.IO (readFile)
import Text.Printf
import qualified Data.Text as Text

import Data.Time.Format
import Data.Time.LocalTime

import Network.Linklater
import Network.Wai.Handler.Warp (run)

import qualified Strive

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

-- Begin Strava API interaction
latestActivity :: Strive.Client -> IO (Maybe Strive.ActivitySummary)
latestActivity stravaClient = do
  let params = Strive.with [ Strive.set Strive.perPage 1 ]
  result <- Strive.getCurrentActivities stravaClient params
  logLine "Making Strava request to /v3/athlete/activities!"
  case result of
    Left _ -> return Nothing
    Right activities -> return $ Just $ head activities

metersPerMile :: Double
metersPerMile = 1609.344

formatActivity :: Strive.ActivitySummary -> String
formatActivity activity = "You last went for a " ++ distanceMiles ++ " mile " ++ type_ ++ " on " ++ date ++ "! Keep at it!"
  where
    type_ = show $ Strive.get Strive.type_ activity
    date = show $ Strive.get Strive.startDate activity
    distance = Strive.get Strive.distance activity
    distanceMiles = printf "%5.1f" (distance / metersPerMile) :: String

-- End Strava API interaction

formatMessage :: Text.Text -> Message
formatMessage text = SimpleMessage icon name channel text

stravaBot :: Strive.Client -> Config -> Maybe Command -> IO Text.Text
stravaBot stravaClient config (Just command) = do
  logLine ("Received a command: " ++ show command)
  maybeActivity <- latestActivity stravaClient
  let message = case maybeActivity of
                  Just activity -> text $ formatActivity activity
                  Nothing -> text "No activities you lazy bum!"
  void $ say (formatMessage message) config
  return $ text "Handled a command!\n"
stravaBot _ _ Nothing = return $ text "Failed to parse incoming command...\n"

readCredentials :: String -> IO String
readCredentials path = filter (/= '\n') <$> readFile path

main :: IO ()
main = do
  slackUrl <- readCredentials "secrets/slack-webhook-url"
  stravaAccessToken <- readCredentials "secrets/strava-access-token"
  stravaClient <- Strive.buildClient (Just $ text stravaAccessToken)
  logLine ("Listening on port " ++ show port)
  run port (slashSimple (stravaBot stravaClient (Config $ text slackUrl)))
  where port = 8000
