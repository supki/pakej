-- | Daemonizing @pakej@ process
module Pakej.Daemon.Daemonize (daemonize) where

import Control.Applicative
import Control.Monad
import System.Directory (getAppUserDataDirectory)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import System.Posix hiding (Ignore)
import Text.Read (readMaybe)
import Text.Printf (printf)

import Pakej.Conf (Existing(..))


-- | Forks, prepares child process to serve as a daemon, then exits
-- with @EXIT_SUCCESS@
daemonize :: Existing -> IO a -> IO b
daemonize prev ioa = do
  forkProcess (void (prepareChild prev *> ioa))
  exitSuccess

-- | Change the working directory to @\/@, set the fmask to @027@,
-- close @stdin@, @stdout@, and @stderr@, create Unix socket file
prepareChild :: Existing -> IO ()
prepareChild prev = do
  changeWorkingDirectory "/"
  pidfile <- appDirectory "pakej" "pakej.pid"
  killPakej pidfile prev
  savePakej pidfile
  setFileCreationMask 0o027
  close [stdInput, stdOutput, stdError]

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
--
-- Return 'Left' if
--
--   * The pidfile does not exist or does not contain a readable number
--
--   * The process with the PID stored in the pidfile does not exist
--
--   * Pakej couldn't terminate the process with the stored PID
killPakej :: FilePath -> Existing -> IO (Either IOError ())
killPakej pidfile prev = tryIOError $ do
  Just pid <- readMaybe <$> readFile pidfile
  pingProcess pid
  case prev of
    Replace -> terminateProcess pid
    Respect -> die (printf "Can't proceed, found running instance: %s" (show pid))

-- | Print the message in stderr and then die with @EXIT_FAILURE@
die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  exitFailure

pingProcess, terminateProcess :: ProcessID -> IO ()
pingProcess      = signalProcess nullSignal
terminateProcess = signalProcess sigTERM

-- | Save current @pakej@ process pid
savePakej :: FilePath -> IO ()
savePakej pidfile = getProcessID >>= writeFile pidfile . show

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
