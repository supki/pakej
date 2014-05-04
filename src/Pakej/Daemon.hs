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
import           Data.Conduit (($=), (=$), (=$=), ($$))
import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import           Data.Conduit.Cereal (conduitGet, conduitPut)
import qualified Data.Conduit.List as CL
import           Data.IORef
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map
import           Data.HashMap.Strict (HashMap)
import           Data.Serialize (get, put)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Version (showVersion)
import           Network
import           Prelude hiding ((.), id, fail)
import           System.Directory (removeFile)
import           System.IO (hClose)
import           System.IO.Error (tryIOError)
import           Text.Printf (printf)

import           Pakej.Conf (Conf, addrs)
import           Pakej.Daemon.Daemonize (daemonize)
import           Pakej.Protocol
import           Pakej.Widget

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
  mapM_ (putStrLn . pretty) (view addrs conf)
 where
  pretty p = "  - " ++ site "localhost" p

listen :: IORef (HashMap Text (Access Text)) -> PortID -> IO Lock
listen ref p = do
  lock <- newLock
  forkFinally listenLoop (\_ -> releaseLock lock)
  return lock
 where
   listenLoop = bracket (preparePort p >> listenOn p) sClose $ \s ->
     forever $ do
       (h, _, _) <- accept s
       forkFinally
        (sourceHandle h $= conduitGet get =$= CL.mapM respond $$ conduitPut put =$ sinkHandle h)
        (\_ -> hClose h)

   respond query = do
     m <- liftIO $ readIORef ref
     return (response m p query)

newtype Lock = Lock { unLock :: MVar () }

newLock :: IO Lock
newLock = Lock <$> newEmptyMVar

acquireLock :: Lock -> IO ()
acquireLock = takeMVar . unLock

releaseLock :: Lock -> IO ()
releaseLock (Lock var) = putMVar var ()

response :: HashMap Text (Access Text) -> PortID -> Request -> Response
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
  :: (Show l, Eq l, Hashable l, Integral n, Applicative m, MonadIO m)
  => IORef (HashMap l (Access v)) -> Widget m l v (Config n) a -> m b
worker ref w = step (unWidget w) Map.empty clockSession_
 where
  step w' m' session' = do
    (dt, session'') <- stepSession session'
    ((_, w''), m'') <- runStateT (stepWire w' dt (Right defaultConfig)) m'
    liftIO $ do
      atomicWriteIORef ref m''
      threadDelay 200000
    step w'' m'' session''
