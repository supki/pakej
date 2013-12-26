module Pakej.Client where

import qualified Data.ByteString as ByteString
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import           Network
import           System.Directory (getAppUserDataDirectory)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, hClose)
import           System.Timeout (timeout)


client :: HostName -> PortID -> String -> IO ()
client n p o = do
  res <- timeout (5 * second) $ do
    h <- connectTo n p
    hPutStrLn h o
    i <- ByteString.hGetLine h
    hClose h
    return (Text.decodeUtf8 i)
  case res of
    Nothing -> exitFailure
    Just m  -> Text.putStrLn m

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)

second :: Int
second = 1000000
