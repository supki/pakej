module Pakej.Client
  ( repl
  , oneshot
  , query
  , exchange
  ) where

import           Control.Concurrent (myThreadId)
import           Control.Monad
import           Control.Exception (bracket, catches)
import           Control.Exception.Lens
import           Data.String (fromString)
import           Data.Text.Lazy (Text)
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
import           Pakej.Daemon (PakejWidget)
import           Pakej.Widget (text)


repl :: HostName -> PortID -> IO a
repl host port = do
  signalHandlers
  forever $ do
    prompt msg
    input <- getLine
    case parseInput input of
      Right q -> do
        res <- exchange host port q
        case res of
          Nothing ->
            hPutStrLn stderr "*** Pakej did not respond"
          Just (Left e) ->
            hPutStrLn stderr ("*** Pakej responded with gibberish: " ++ e)
          Just (Right (DQuery response)) ->
            print response
          Just (Right (DStatus response)) ->
            print response
      Left (InvalidQuery q) ->
        hPutStrLn stderr ("*** Invalid query: " ++ show q)
      Left (UnknownCommand command) ->
        hPutStrLn stderr ("*** Unknown command: " ++ show command)
   `catches`
    [ handler_ (_IOException.errorType._EOF) $ do
        putStrLn "\nLeaving Pakej alone."
        throwingM _ExitSuccess ()
    , handler_ _UserInterrupt $
        putStr "\n"
    ]
 where msg = printf "pakej %s >>> " (site host port)

oneshot :: HostName -> PortID -> Request -> IO ()
oneshot host port q = do
  res <- exchange host port q
  case res of
    Just (Right (DQuery (Just response))) ->
      Text.putStrLn response
    Just (Right (DStatus response)) ->
      Text.putStrLn (Text.unwords response)
    _ ->
      exitFailure

query :: HostName -> PortID -> Text -> PakejWidget Text
query h p q = text $ do
  res <- exchange h p (CQuery q)
  case res of
    Just (Right (DQuery (Just t))) -> return t
    _ -> exitFailure

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
  | [q] <- words s                  = Right (CQuery (fromString q))
  | otherwise                       = Left (InvalidQuery s)

data InvalidInput =
    InvalidQuery String
  | UnknownCommand String
    deriving (Show, Eq)
