{-# LANGUAGE NamedFieldPuns #-}
module Pakej.Daemon (daemon) where

import           Control.Applicative
import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Control.Exception (IOException, bracket)
import           Control.Monad (forM, forM_, forever, void)
import qualified Data.ByteString.Lazy as ByteString
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Map (Map)
import           Data.Monoid (Monoid(..))
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import           Network
import           Prelude hiding (fail)
import           System.Directory (removeFile)
import           System.IO (hGetLine, hClose)
import           System.IO.Error (catchIOError, tryIOError)
import           Text.Printf (printf)

import           Pakej.Action
import           Pakej.Conf (Previous)
import           Pakej.Daemon.Daemonize (daemonize)

{-# ANN module "HLint: Avoid lambda" #-}


daemon :: [PortID] -> Previous -> [Pakejee Text] -> IO b
daemon ps t pjs =
  daemonize t $ do
    refs <- makeRefs pjs
    forM_ pjs (installHook refs)
    forM_ ps  (listen refs)
    forever $
      threadDelay 1000000

listen :: (Map String (IORef (Pakejer Text))) -> PortID -> IO ThreadId
listen refs p = forkIO $
  bracket (preparePort p >> listenOn p) sClose $ \s -> do
    forever $
      bracket (accept s) (\(h, _, _) -> hClose h) $ \(h, _, _) -> do
      k <- hGetLine h
      case Map.lookup k refs of
        Just ref -> do
          Pakejer a r <- readIORef ref
          case (a, p) of
            (Private, PortNumber _) -> hClose h
            (_, _) -> ByteString.hPut h (Text.encodeUtf8 (unResult r))
        Nothing -> hClose h
     `catchIOError` \e -> do
      threadDelay 100000
      print e

preparePort :: PortID -> IO (Either IOError ())
preparePort (UnixSocket s) = tryIOError (removeFile s)
preparePort _              = return (Right ())

data Pakejer a = Pakejer
  { _access :: Access
  , _result :: Result a
  } deriving (Show, Eq)

data Result a = Fail { unResult :: a } | Success { unResult :: a }
  deriving (Show, Eq)

makeRefs :: Monoid r => [Pakejee r] -> IO (Map String (IORef (Pakejer r)))
makeRefs ps = do
  xs <- forM ps $ \p -> do
    ref <- newIORef (Pakejer (access p) (Fail mempty))
    return (name p, ref)
  return (Map.fromList xs)

installHook :: Map String (IORef (Pakejer Text)) -> Pakejee Text -> IO ()
installHook refs p =
  case Map.lookup (name p) refs of
    Nothing  -> return ()
    Just ref -> void . forkIO . forever $ do
      eer <- tryIO refs ref (action p)
      case eer of
        Right _ -> return ()
        Left  e -> printf "%s failed: %s" (name p) (show e)

tryIO
  :: Map String (IORef (Pakejer Text))
  -> IORef (Pakejer Text)
  -> Action Text
  -> IO (Either IOException Text)
tryIO _ ref (IO ior t) = do
  r <- Text.strip <$> ior
  if Text.null r
    then atomicModifyIORef'_ ref (\p -> p { _result = Fail (unResult (_result p)) })
    else atomicModifyIORef'_ ref (\p -> p { _result = Success r })
  threadDelay t
  return (Right r)
 `catchIOError`
  \x -> do
    threadDelay defaultTimeout
    return (Left x)
tryIO refs ref (Group ns sep) = do
  let refs' = mapMaybe (\n -> Map.lookup n refs) ns
  xs <- mapM readIORef refs'
  let r = Text.intercalate sep (mapMaybe succeeded xs)
  atomicModifyIORef'_ ref (\p -> p { _result = Success r })
  threadDelay defaultTimeout
  return (Right r)
 `catchIOError`
  \x -> return (Left x)

atomicModifyIORef'_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'_ ref f = atomicModifyIORef' ref (\p -> (f p , ()))

succeeded :: Pakejer a -> Maybe a
succeeded (Pakejer _ (Success x)) = Just x
succeeded _                       = Nothing
