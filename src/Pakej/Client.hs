module Pakej.Client where

import           Data.Text (Text)
import qualified Data.Text.IO as Text
import           Network
import           System.Directory (getAppUserDataDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.Timeout (timeout)

import           Pakej.Communication


client :: HostName -> PortID -> Text -> IO ()
client n p query = do
  res <- timeout (5 * second) $ do
    h <- connectTo n p
    send h (CQuery query)
    recv h
  case res of
    Nothing ->
      exitFailure
    Just (Left _) ->
      exitFailure
    Just (Right (DResponse response)) ->
      Text.putStrLn response

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)

second :: Int
second = 1000000
