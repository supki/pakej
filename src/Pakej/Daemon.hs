{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Pakej.Daemon
  ( PakejWidget
  , daemon
  ) where

import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (forM_, forever)
import           Control.Monad.Trans.Writer (WriterT(..))
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Wire hiding (loop)
import           Data.IORef
import           Data.Monoid (Endo(..))
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Network
import           Prelude hiding ((.), id, fail)
import           System.Directory (removeFile)
import           System.IO (hClose)
import           System.IO.Error (catchIOError, tryIOError)

import           Pakej.Widget
import           Pakej.Communication
import           Pakej.Conf (Existing)
import           Pakej.Daemon.Daemonize (daemonize)

-- | A highly monomorphic 'Widget' type used by Pakej itself
type PakejWidget = Widget IO Text Text (Config Integer)

daemon :: [PortID] -> Existing -> PakejWidget a -> IO b
daemon ps t w =
  daemonize t $ do
    ref <- newIORef Map.empty
    forkIO (worker ref w)
    forM_ ps (listen ref)
    forever $
      threadDelay 1000000

listen :: IORef (Map Text (Access Text)) -> PortID -> IO ThreadId
listen ref p = forkIO $
  bracket (preparePort p >> listenOn p) sClose $ \s ->
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
  :: (Show l, Ord l, Integral n, MonadIO m)
  => IORef (Map l (Access v)) -> Widget m l v (Config n) a -> m b
worker ref w = step w Map.empty clockSession
 where
  step w' m' session' = do
    ((_, w'', session''), f) <- runWriterT (stepSession w' session' defaultConfig)
    let m'' = appEndo f m'
    liftIO $ do
      atomicWriteIORef ref m''
      threadDelay 200000
    step w'' m'' session''
