module Utils.Log (line) where

import Data.Time.Format
import Data.Time.LocalTime

defaultFormat :: String
defaultFormat = "%F %T %Z"

-- Log to stdout with a local timestamp.
-- TODO: Should get a real logging framework...
line :: String -> IO ()
line s = do
  zonedTime <- getZonedTime
  let timestamp = formatTime defaultTimeLocale defaultFormat zonedTime
  putStrLn ("[" ++ timestamp ++ "] " ++ s)
