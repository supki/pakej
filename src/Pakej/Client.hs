module Pakej.Client
  ( repl
  , client
  ) where

import           Control.Concurrent (myThreadId)
import           Control.Monad
import           Control.Exception (bracket, catches)
import           Control.Exception.Lens
import           Data.String (fromString)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import           Network
import           System.Exit (exitFailure)
import           System.Exit.Lens (_ExitSuccess)
import           System.IO (Handle, hClose, hFlush, hPutStrLn, stdout, stderr)
import           System.IO.Error.Lens (errorType, _EOF)
import           System.Posix.Signals (installHandler, keyboardSignal, Handler(..))
import           System.Timeout (timeout)
import           Text.Printf (printf)

import           Pakej.Communication


repl :: HostName -> PortID -> IO a
repl host port = do
  signalHandlers
  forever $ do
    prompt msg
    input <- getLine
    case parseInput input of
      Right query -> do
        res <- exchange host port query
        case res of
          Nothing ->
            hPutStrLn stderr "*** Pakej did not respond"
          Just (Left e) ->
            hPutStrLn stderr ("*** Pakej responded with gibberish: " ++ e)
          Just (Right (DQuery response)) ->
            print response
          Just (Right (DStatus response)) ->
            print response
      Left (InvalidQuery query) ->
        hPutStrLn stderr ("*** Invalid query: " ++ show query)
      Left (UnknownCommand command) ->
        hPutStrLn stderr ("*** Unknown command: " ++ show command)
   `catches`
    [ handler_ (_IOException.errorType._EOF) $ do
        putStrLn "\nLeaving Pakej alone."
        throwingM _ExitSuccess ()
    , handler_ _UserInterrupt $
        putStr "\n"
    ]
 where msg = printf "pakej %s:%s >>> " host (prettyPort port)

client :: HostName -> PortID -> Request -> IO ()
client host port query = do
  res <- exchange host port query
  case res of
    Just (Right (DQuery (Just response))) ->
      Text.putStrLn response
    Just (Right (DStatus response)) ->
      Text.putStrLn (Text.unwords response)
    _ ->
      exitFailure

-- | Allow user to press ^C twice without REPL dying because of the default SIGINT handler
signalHandlers :: IO ()
signalHandlers = void $ do
  tid <- myThreadId
  installHandler keyboardSignal (Catch (throwingTo tid _UserInterrupt ())) Nothing

-- | Send the query to the Pakej instance
--
--   * @'Nothing'@ means Pakej did not respond in 5 second timeout
--   * @'Left' e@ means Pakej did respond with garbage @e@
exchange :: (Send a, Recv b) => HostName -> PortID -> a -> IO (Maybe (Either String b))
exchange host port command = timeout (5 * second) . connect host port $ communicate command
 where second = 1000000

-- | Pretty print the port to use in prompt message
prettyPort :: PortID -> String
prettyPort (PortNumber n) = show n
prettyPort (Service s)    = s
prettyPort (UnixSocket s) = s

connect :: HostName -> PortID -> (Handle -> IO a) -> IO a
connect n p = bracket (connectTo n p) hClose

prompt :: String ->  IO ()
prompt m = do
  putStr m
  hFlush stdout

parseInput :: String -> Either InvalidInput Request
parseInput s
  | (":", "stat") <- span (== ':') s = Right CStatus
  | (":", c) <- span (== ':') s      = Left (UnknownCommand c)
  | [query] <- words s               = Right (CQuery (fromString query))
  | otherwise                       = Left (InvalidQuery s)

data InvalidInput =
    InvalidQuery String
  | UnknownCommand String
    deriving (Show, Eq)
