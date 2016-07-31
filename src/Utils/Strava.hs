module Utils.Strava
    ( Client
    , client
    , formatActivity
    , latestActivity
    ) where

import Text.Printf
import qualified Data.Text as T
import qualified Strive

import qualified Utils.Log as Log

type Client = Strive.Client

client :: T.Text -> IO Client
client accessToken = Strive.buildClient (Just accessToken)

formatActivity :: Strive.ActivitySummary -> String
formatActivity activity = "You last went for a " ++ distanceMiles ++ " mile " ++ type_ ++ " on " ++ date ++ "! Keep at it!"
  where
    type_ = show $ Strive.get Strive.type_ activity
    date = show $ Strive.get Strive.startDate activity
    distance = Strive.get Strive.distance activity
    distanceMiles = printf "%5.1f" (distance / metersPerMile) :: String

latestActivity :: Strive.Client -> IO (Maybe Strive.ActivitySummary)
latestActivity stravaClient = do
  let params = Strive.with [ Strive.set Strive.perPage 1 ]
  result <- Strive.getCurrentActivities stravaClient params
  Log.line "Making Strava request to /v3/athlete/activities!"
  case result of
    Left _ -> return Nothing
    Right activities -> return $ Just $ head activities

metersPerMile :: Double
metersPerMile = 1609.344
