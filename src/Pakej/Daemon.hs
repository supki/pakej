{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Pakej.Daemon (daemon) where

import           Control.Applicative
import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Control.Exception (bracket)
import           Control.Monad (forM_, forever)
import           Control.Monad.Trans.Writer (WriterT(..), tell)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Wire
import           Data.Foldable (for_)
import           Data.IORef
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
import           Pakej.Conf (Previous)
import           Pakej.Daemon.Daemonize (daemonize)


daemon :: Integral n => [PortID] -> Previous -> Widget IO Text Text (Config n) a -> IO b
daemon ps t w =
  daemonize t $ do
    ref <- newIORef Map.empty
    forkIO (worker ref w)
    forM_ ps (listen ref)
    forever $
      threadDelay 1000000

listen :: IORef (Map (Label Text) Text) -> PortID -> IO ThreadId
listen ref p = forkIO $
  bracket (preparePort p >> listenOn p) sClose $ \s ->
    forever $
      bracket (accept s) (\(h, _, _) -> hClose h) $ \(h, _, _) -> do
      k <- recv h
      case k of
        Right query -> do
          m   <- readIORef ref
          res <- respond m p query
          for_ res (send h)
          hClose h
        Left _ ->
          hClose h
     `catchIOError` \e -> do
      threadDelay 100000
      print e

respond :: Map (Label Text) Text -> PortID -> Request -> IO (Maybe Response)
respond m p = \case
  CQuery key -> case (lookupLabel key m, p) of
    (Just (Private _, _), PortNumber _) -> return Nothing
    (Just (_, r), _) -> do
      return (Just (DQuery (Text.replace (Text.pack "\n") (Text.pack "") r)))
    (Nothing, _) -> return Nothing
  CStatus -> case p of
    PortNumber _ ->
      let
        disclosed = [k | Public k <- Map.keys m]
      in
        return (Just (DStatus disclosed))
    _ ->
      return (Just (DStatus (map unLabel (Map.keys m))))

preparePort :: PortID -> IO (Either IOError ())
preparePort (UnixSocket s) = tryIOError (removeFile s)
preparePort _              = return (Right ())

worker
  :: (Show l, Ord l, Integral n, MonadIO m) => IORef (Map (Label l) v) -> Widget m l v (Config n) a -> m b
worker ref w = step w Map.empty clockSession
 where
  step w' m' session' = do
    ((_, w'', session''), m'') <- runWriterT $ do
      res <- stepSession w' session' defaultConfig
      tell m'
      return res
    liftIO $ do
      atomicWriteIORef ref m''
      threadDelay 200000
    step w'' m'' session''

lookupLabel :: Ord l => l -> Map (Label l) v -> Maybe (Label l, v)
lookupLabel l m = go (Public l) m <|> go (Private l) m
 where go l' = fmap (\x -> (l', x)) . Map.lookup l'
