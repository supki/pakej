{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Pakej.Client
  ( oneshot
  , query
  , repl
  ) where

import           Control.Concurrent (myThreadId)
import           Control.Monad
import           Control.Exception (bracket, catches)
import           Control.Exception.Lens
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Conduit (Producer, Consumer, (=$), (=$=), ($$), yield)
import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import           Data.Conduit.Cereal (sinkGet, conduitGet, conduitPut)
import qualified Data.Conduit.List as CL
import           Data.Function (fix)
import           Data.Serialize (get, put)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Network
import           System.Exit (exitFailure)
import           System.Exit.Lens (_ExitSuccess)
import           System.IO (Handle, hClose, hFlush, hPutStrLn, stdout, stderr)
import           System.IO.Error.Lens (errorType, _EOF)
import           System.Posix.Signals (installHandler, keyboardSignal, Handler(..))
import           System.Timeout (timeout)
import           Text.Printf (printf)

import           Pakej.Daemon (PakejWidget)
import           Pakej.Protocol
import           Pakej.Widget (text)


oneshot :: HostName -> PortID -> Request -> IO ()
oneshot host port q = do
  res <- timeout (5 * 1000000) . connect host port $ \h -> do
    yield q $$ conduitPut put =$ sinkHandle h
    sourceHandle h $$ sinkGet get
  case res of
    Just (DQuery (Just response)) ->
      Text.putStrLn response
    Just (DStatus response) ->
      Text.putStrLn (Text.unwords response)
    _ ->
      exitFailure

query :: HostName -> PortID -> Text -> PakejWidget Text
query host port q = text $ do
  res <- timeout (5 * 1000000) . connect host port $ \h -> do
    yield (CQuery q) $$ conduitPut put =$ sinkHandle h
    sourceHandle h $$ sinkGet get
  case res of
    Just (DQuery (Just t)) -> return t
    _ -> exitFailure

repl :: HostName -> PortID -> IO a
repl host port = do
  signalHandlers
  connect host port $ \h ->
    forever $ do
      userInput msg $$ conduitPut put =$ sinkHandle h
      sourceHandle h $$ conduitGet get =$= CL.isolate 1 =$ userOutput
     `catches`
       [ handler_ (_IOException.errorType._EOF) $ do
           putStrLn "\nLeaving Pakej alone."
           throwingM _ExitSuccess ()
       , handler_ _UserInterrupt $
           putStr "\n"
       ]
 where msg = printf "pakej %s >>> " (site host port)

userInput :: MonadIO m => String -> Producer m Request
userInput msg = fix $ \loop -> do
  liftIO $ prompt msg
  input <- liftIO $ getLine
  case parseInput input of
    Right q -> yield q
    Left e -> do
      case e of
        InvalidQuery q -> do
          liftIO $ hPutStrLn stderr ("*** Invalid query: " ++ show q)
        UnknownCommand command -> do
          liftIO $ hPutStrLn stderr ("*** Unknown command: " ++ show command)
      loop

userOutput :: MonadIO m => Consumer Response m ()
userOutput = CL.mapM_ $ liftIO . \case DQuery r -> print r; DStatus r -> print r

-- | Allow user to press ^C twice without REPL dying because of the default SIGINT handler
signalHandlers :: IO ()
signalHandlers = void $ do
  tid <- myThreadId
  installHandler keyboardSignal (Catch (throwingTo tid _UserInterrupt ())) Nothing

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
  | [q] <- words s                   = Right (CQuery (fromString q))
  | otherwise                       = Left (InvalidQuery s)

data InvalidInput =
    InvalidQuery String
  | UnknownCommand String
    deriving (Show, Eq)
