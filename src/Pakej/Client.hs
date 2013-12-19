module Pakej.Client where

import qualified Data.ByteString as ByteString
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import           Network
import           System.Directory (getAppUserDataDirectory)
import           System.FilePath ((</>))
import           System.IO (hPutStrLn, hClose)


client :: HostName -> PortID -> String -> IO ()
client n p o = do
  h <- connectTo n p
  hPutStrLn h o
  i <- ByteString.hGetLine h
  hClose h
  Text.putStrLn (Text.decodeUtf8 i)

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
