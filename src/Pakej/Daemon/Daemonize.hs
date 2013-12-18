-- | Daemonizing @pakej@ process
module Pakej.Daemon.Daemonize (daemonize) where

import Control.Applicative
import Control.Monad
import System.Posix
import System.Directory (getAppUserDataDirectory, removeFile)
import System.Exit (exitSuccess)
import System.FilePath ((</>))
import System.IO.Error (tryIOError)
import Text.Read (readMaybe)


-- | Forks, prepares child process to serve as a daemon, then exits
-- with @EXIT_SUCCESS@
daemonize :: (String -> IO a) -> IO b
daemonize ioa = do
  forkProcess (void (prepareChild >>= ioa))
  exitSuccess

-- | Change the working directory to @\/@, set the fmask to @027@,
-- close @stdin@, @stdout@, and @stderr@, create Unix socket file
prepareChild :: IO String
prepareChild = do
  changeWorkingDirectory "/"
  pidfile <- appDirectory "pakej" "pakej.pid"
  killPakej pidfile
  savePakej pidfile
  setFileCreationMask 0o027
  close [stdInput, stdOutput, stdError]
  socket <- appDirectory "pakej" "pakej.sock"
  tryIOError $ removeFile socket
  return socket

-- | Closes fds
close :: [Fd] -> IO ()
close fds = do
  devNull <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
  mapM_ (redirect devNull) fds
 where
  redirect fd' fd = do
    closeFd fd
    dupTo fd' fd

-- | Kill running @pakej@ process if exists
killPakej :: FilePath -> IO (Either IOError Int)
killPakej pidfile = tryIOError $ do
  Just pid <- readMaybe <$> readFile pidfile
  signalProcess sigTERM pid
  return (fromIntegral pid)

-- | Save current @pakej@ process pid
savePakej :: FilePath -> IO ()
savePakej pidfile = do
  pid <- getProcessID
  writeFile pidfile (show pid)

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
