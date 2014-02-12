module Pakej.Client
  ( client
  , repl
  ) where

import           Control.Monad
import           Control.Exception (bracket)
import           Data.String (fromString)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text
import           Network
import           System.Exit (exitFailure)
import           System.IO (Handle, hClose, hFlush, hPutStrLn, stdout, stderr)
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

second :: Int
second = 1000000

repl :: HostName -> PortID -> IO ()
repl n p = forever $ do
  prompt ">>> "
  raw <- getLine
  case parseCommand raw of
    Just command ->
      connect n p $ \h -> do
        send h command
        res <- timeout (5 * second) (recv h)
        case res of
          Nothing ->
            hPutStrLn stderr "*** Pakej did not respond"
          Just (Left e) ->
            hPutStrLn stderr ("*** Pakej responded with gibberish: " ++ e)
          Just (Right (DQuery response)) ->
            Text.putStrLn response
          Just (Right (DStatus commands)) ->
            Text.putStrLn (Text.unwords commands)
    Nothing ->
      hPutStrLn stderr ("*** Unknown command: `" ++ raw ++ "'")

connect :: HostName -> PortID -> (Handle -> IO ()) -> IO ()
connect n p = bracket (connectTo n p) hClose

prompt :: String ->  IO ()
prompt m = do
  putStr m
  hFlush stdout

parseCommand :: String -> Maybe Request
parseCommand raw
  | (":", "stat") <- span (== ':') raw = Just CStatus
  | [query]       <- words raw         = Just (CQuery (fromString query))
  | otherwise = Nothing
