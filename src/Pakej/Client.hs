module Pakej.Client where

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Network
import           System.Directory (getAppUserDataDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.Timeout (timeout)

import           Pakej.Communication


client :: HostName -> PortID -> Request -> IO ()
client n p command = do
  res <- timeout (5 * second) $ do
    h <- connectTo n p
    send h command
    recv h
  case res of
    Nothing ->
      exitFailure
    Just (Left _) ->
      exitFailure
    Just (Right (DQuery response)) ->
      Text.putStrLn response
    Just (Right (DStatus commands)) ->
      Text.putStrLn (Text.unwords commands)

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)

second :: Int
second = 1000000
