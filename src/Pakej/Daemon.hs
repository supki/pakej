{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
module Pakej.Daemon (daemon) where

import           Control.Applicative
import           Control.Concurrent (ThreadId, forkIO, threadDelay)
import           Control.Exception (IOException, bracket)
import           Control.Monad (forM, forM_, forever, void)
import           Data.IORef
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.Map (Map)
import           Data.Monoid (Monoid(..))
import qualified Data.Text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Network
import           Prelude hiding (fail)
import           System.Directory (removeFile)
import           System.IO (hClose)
import           System.IO.Error (catchIOError, tryIOError)
import           Text.Printf (printf)

import           Pakej.Action
import           Pakej.Communication
import           Pakej.Conf (Previous)
import           Pakej.Daemon.Daemonize (daemonize)


daemon :: [PortID] -> Previous -> [Pakej Text] -> IO b
daemon ps t pjs =
  daemonize t $ do
    refs <- makeRefs pjs
    forM_ pjs (installHook refs)
    forM_ ps  (listen refs)
    forever $
      threadDelay 1000000

listen :: Map String (PakejerRef Text) -> PortID -> IO ThreadId
listen refs p = forkIO $
  bracket (preparePort p >> listenOn p) sClose $ \s ->
    forever $
      bracket (accept s) (\(h, _, _) -> hClose h) $ \(h, _, _) -> do
      k <- recv h
      case k of
        Right query -> do
          res <- respond refs p query
          case res of
            Just res' -> send h res'
            Nothing -> hClose h
        Left _ ->
          hClose h
     `catchIOError` \e -> do
      threadDelay 100000
      print e

respond :: Map String (PakejerRef Text) -> PortID -> Request -> IO (Maybe Response)
respond refs p = \case
  CQuery query -> case Map.lookup (Data.Text.unpack query) refs of
    Just ref ->
      case (ref, p) of
        (Private _, PortNumber _) -> return Nothing
        (_, _) -> do
          r <- readPakejerRef ref
          return (Just (DQuery (Text.toStrict (unResult r))))
    Nothing -> return Nothing
  CStatus -> case p of
    PortNumber _ ->
      let
        disclosed = map fst . filter (isPublic . snd) $ Map.toList refs
      in
        return (Just (DStatus (map Data.Text.pack disclosed)))
    _ ->
      return (Just (DStatus (map Data.Text.pack (Map.keys refs))))

preparePort :: PortID -> IO (Either IOError ())
preparePort (UnixSocket s) = tryIOError (removeFile s)
preparePort _              = return (Right ())

type PakejerRef r = Access (IORef (Pakejer r))

data Pakejer a = Fail { unResult :: a } | Success { unResult :: a }
  deriving (Show, Eq)

makeRefs :: Monoid r => [Pakej r] -> IO (Map String (PakejerRef r))
makeRefs ps = do
  xs <- forM ps $ \(Pakej p) -> do
    ref <- newIORef (Fail mempty)
    return (name p, ref <$ p)
  return (Map.fromList xs)

installHook :: Map String (PakejerRef Text) -> Pakej Text -> IO ()
installHook refs (Pakej p) =
  case Map.lookup (name p) refs of
    Nothing  -> return ()
    Just ref -> void . forkIO . forever $ do
      eer <- tryIO refs ref (action p)
      case eer of
        Right _ -> return ()
        Left  e -> printf "%s failed: %s" (name p) (show e)

tryIO
  :: Map String (PakejerRef Text)
  -> PakejerRef Text
  -> Action m Text
  -> IO (Either IOException Text)
tryIO _ ref (IO ior t) = do
  r <- Text.strip <$> ior
  if Text.null r
    then atomicModifyPakejerRef'_ ref (Fail . unResult)
    else atomicWritePakejerRef ref (Success r)
  threadDelay t
  return (Right r)
 `catchIOError`
  \x -> do
    threadDelay defaultTimeout
    return (Left x)
tryIO refs ref (Group ns sep) = do
  let refs' = mapMaybe (\n -> Map.lookup n refs) ns
  xs <- mapM readPakejerRef refs'
  let r = Text.intercalate sep (mapMaybe succeeded xs)
  atomicWritePakejerRef ref (Success r)
  threadDelay defaultTimeout
  return (Right r)
 `catchIOError`
  \x -> return (Left x)

succeeded :: Pakejer a -> Maybe a
succeeded (Success x) = Just x
succeeded _           = Nothing

readPakejerRef :: PakejerRef a -> IO (Pakejer a)
readPakejerRef = readIORef . access

atomicWritePakejerRef :: PakejerRef a -> Pakejer a -> IO ()
atomicWritePakejerRef ref = atomicModifyPakejerRef'_ ref . const

atomicModifyPakejerRef'_ :: PakejerRef a -> (Pakejer a -> Pakejer a) -> IO ()
atomicModifyPakejerRef'_ ref f = atomicModifyIORef' (access ref) (\p -> (f p , ()))

isPublic :: Access r -> Bool
isPublic (Public _) = True
isPublic _          = False
