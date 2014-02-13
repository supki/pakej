{-# LANGUAGE DefaultSignatures #-}
-- | Client-Daemon communication
module Pakej.Communication
  ( -- * Communication
    send, recv
    -- * Commands
  , Communicate, Request(..), Response(..)
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Exception (evaluate)
import qualified Data.ByteString.Lazy as ByteString
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Encoding as Text
import           Data.Serialize (Serialize(..), getWord8, putWord8, encodeLazy, decodeLazy)
import           Data.Traversable (traverse)
import           System.IO (Handle)
import           Text.Printf (printf)


class Communicate m where
  send :: Handle -> m -> IO ()
  default send :: Serialize m => Handle -> m -> IO ()
  send h = ByteString.hPut h . encodeLazy

  recv :: Handle -> IO (Either String m)
  default recv :: Serialize m => Handle -> IO (Either String m)
  recv = evaluate <=< fmap decodeLazy . ByteString.hGetContents

instance Communicate Request
instance Communicate Response


data Request =
    CQuery Text
  | CStatus
    deriving (Show, Eq)

instance Serialize Request where
  put (CQuery t) = do
    putWord8 0
    put (Text.encodeUtf8 t)
  put CStatus =
    putWord8 1
  get = do
    w <- getWord8
    case w of
      0 -> do
        Right t <- Text.decodeUtf8' <$> get
        return (CQuery t)
      1 -> return CStatus
      _ -> fail (printf "Unknown Pakej.Command.Client value tag: %d" w)

data Response =
    DQuery (Maybe Text)
  | DStatus [Text]
    deriving (Show, Eq)

instance Serialize Response where
  put (DQuery t) = do
    putWord8 0
    put (fmap Text.encodeUtf8 t)
  put (DStatus ts) = do
    putWord8 1
    put (map Text.encodeUtf8 ts)
  get = do
    w <- getWord8
    case w of
      0 -> do
        Right t <- traverse Text.decodeUtf8' <$> get
        return (DQuery t)
      1 -> do
        Right ts <- traverse Text.decodeUtf8' <$> get
        return (DStatus ts)
      _ -> fail (printf "Unknown Pakej.Command.Daemon value tag: %d" w)
