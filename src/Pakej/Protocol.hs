{-# LANGUAGE DefaultSignatures #-}
-- | Client-Daemon communication
module Pakej.Protocol
  ( Request(..)
  , Response(..)
  , site
  ) where

import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Serialize (Serialize(..), getWord8, putWord8)
import           Data.Traversable (traverse)
import           Network (HostName, PortID(..))
import           Text.Printf (printf)


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

-- | Pretty print the hostname and port to use in greeting messages
site :: HostName -> PortID -> String
site h p = printf "%s:%s" h (pretty p)
 where
  pretty (PortNumber n) = show n
  pretty (Service s)    = s
  pretty (UnixSocket s) = s
