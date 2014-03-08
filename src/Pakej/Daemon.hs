{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Pakej.Daemon
  ( PakejWidget
  , daemon
  ) where

import           Control.Concurrent (forkIO, forkFinally, threadDelay)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad (forever)
import           Control.Monad.Trans.State.Strict (StateT(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Wire hiding (loop)
import           Data.IORef
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Version (showVersion)
import           Network
import           Prelude hiding ((.), id, fail)
import           System.Directory (removeFile)
import           System.IO (hClose)
import           System.IO.Error (catchIOError, tryIOError)
import           Text.Printf (printf)

import           Pakej.Widget
import           Pakej.Communication
import           Pakej.Conf (Conf, addrs)
import           Pakej.Daemon.Daemonize (daemonize)

import           Paths_pakej (version)

-- | A highly monomorphic 'Widget' type used by Pakej itself
type PakejWidget = Widget IO Text Text (Config Integer)

daemon :: Conf -> PakejWidget a -> IO ()
daemon conf w = do
  greeting conf
  daemonize conf $ do
    ref <- newIORef Map.empty
    forkIO (worker ref w)
    locks <- mapM (listen ref) (view addrs conf)
    mapM_ acquireLock locks

greeting :: Conf -> IO ()
greeting conf = do
  printf "pakej %s, listening on:\n" (showVersion version)
  mapM_ (putStrLn . pretty)  (view addrs conf)
 where
  pretty p = "  - " ++ site "localhost" p

listen :: IORef (Map Text (Access Text)) -> PortID -> IO Lock
listen ref p = do
  lock <- newLock
  forkFinally listenLoop (\_ -> releaseLock lock)
  return lock
 where
   listenLoop = bracket (preparePort p >> listenOn p) sClose $ \s ->
     forever $
       bracket (accept s) (\(h, _, _) -> hClose h) $ \(h, _, _) -> do
         k <- recv h
         case k of
           Right query -> do
             m <- readIORef ref
             send h (response m p query)
           Left _ ->
             return ()
        `catchIOError` \e -> do
         threadDelay 100000
         print e

newtype Lock = Lock { unLock :: MVar () }

newLock :: IO Lock
newLock = Lock <$> newEmptyMVar

acquireLock :: Lock -> IO ()
acquireLock = takeMVar . unLock

releaseLock :: Lock -> IO ()
releaseLock (Lock var) = putMVar var ()

response :: Map Text (Access Text) -> PortID -> Request -> Response
response m p = \case
  CQuery key -> case (Map.lookup key m, p) of
    (Just (Private _), PortNumber _) ->
      DQuery Nothing
    (Just r, _) -> do
      DQuery (Just (Text.replace (Text.pack "\n") (Text.pack "") (unAccess r)))
    (Nothing, _) ->
      DQuery Nothing
  CStatus -> case p of
    PortNumber _ ->
      DStatus [k | (k, Public _) <- Map.toList m]
    _ ->
      DStatus (Map.keys m)

preparePort :: PortID -> IO (Either IOError ())
preparePort (UnixSocket s) = tryIOError (removeFile s)
preparePort _              = return (Right ())

worker
  :: (Show l, Ord l, Integral n, Applicative m, MonadIO m)
  => IORef (Map l (Access v)) -> Widget m l v (Config n) a -> m b
worker ref w = step (unWidget w) Map.empty clockSession_
 where
  step w' m' session' = do
    (dt, session'') <- stepSession session'
    ((_, w''), m'') <- runStateT (stepWire w' dt (Right defaultConfig)) m'
    liftIO $ do
      atomicWriteIORef ref m''
      threadDelay 200000
    step w'' m'' session''
