{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A parser for @\/proc\/meminfo@
module Pakej.Widget.Memory
  ( -- * Memory metadata
#ifdef TEST
    Mem(..)
#else
    Mem
#endif
  , Query
  , widget
    -- ** Make sense of memory metadata
  , ratio
  , total
  , available
  , used
  , lookup
    -- ** Memory metadata parser
  , getData
#ifdef TEST
  , parseData
  , parseLine
  , memoryDataError
#endif
  ) where

import           Control.Applicative
import           Data.Int (Int64)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Traversable (traverse)
import           Prelude hiding (lookup)
import           System.IO.Error (catchIOError)

import           Pakej.Daemon (PakejWidget)
import           Pakej.Widget (text)


-- | Abstract data type representing /proc/meminfo contents
newtype Mem = Mem { unMem :: HashMap Text Int64 } deriving (Show, Eq)

-- | Memory metadata query
type Query a = Mem -> Maybe a

-- | Construct a 'Text' widget from the initial value and the memory metadata query
widget
  :: Text       -- Initial widget value
  -> Query Text -- The result of this query is used as the widget value
  -> PakejWidget Text
widget z q = text $ either (const z) (maybe z id . q) <$> getData "/proc/meminfo"
{-# ANN widget ("HLint: ignore Use fromMaybe" :: String) #-}

-- | The ratio between the results of two memory metadata queries
ratio :: Fractional a => Query a -> Query a -> Query a
ratio = (liftA2.liftA2) (/)

-- | Total amount of memory installed on the machine
total :: Num a => Query a
total = lookup "MemTotal"

-- | Amount of memory available for consumption
--
-- Accurate result requires newer kernels. On older kernels it downgrades to
-- approximate calculations
available :: Num a => Query a
available mem = lookup "MemAvailable" mem <|> availableApprox mem

availableApprox :: Num a => Query a
availableApprox mem = fmap sum (traverse (\k -> lookup k mem) ["MemFree", "Buffers", "Cached"])
{-# ANN availableApprox ("HLint: ignore Avoid lambda" :: String) #-}

-- | Amount of memory already in use
--
-- Accurate result requires newer kernels. On older kernels it downgrades to
-- approximate calculations
used :: Num a => Query a
used = (liftA2.liftA2) (-) total available

-- | Generic memory metadata lookup
lookup :: Num a => Text -> Query a
lookup k = fmap fromIntegral . HashMap.lookup k . unMem

-- | Parse @\/proc\/meminfo@ (or any file with the same format) data
-- to the key-value mapping, e.g. if
--
-- @
-- % cat \/proc\/meminfo
-- MemTotal:        3912112 kB
-- MemFree:          170128 kB
-- @
--
-- then
--
-- >>> getData "/proc/meminfo"
-- Right (fromList [("MemTotal", 3912112), ("MemFree", 170128), ...])
--
-- If any problems are encountered while parsing it returns the first one
-- wrapped in 'Left'
--
-- Catches I/O exceptions thrown upon opening and reading the file and presents
-- their error message wrapped in 'Left'
getData :: FilePath -> IO (Either Text Mem)
getData = handleIOError (return . memoryDataError . Text.pack . show) . fmap parseData . Text.readFile

handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError = flip catchIOError

parseData :: Text -> Either Text Mem
parseData = fmap (Mem . HashMap.fromList) . parseFile

parseFile :: Text -> Either Text [(Text, Int64)]
parseFile = traverse parseLine . Text.lines

parseLine :: Text -> Either Text (Text, Int64)
parseLine l = case Text.words l of
  (pre : n : _)
    | Just pre' <- Text.stripSuffix ":" pre
    , Right (n', _) <- Text.decimal n -> Right (pre', n')
  _ -> memoryDataError $ "bad line: " <> l

memoryDataError :: Text -> Either Text a
memoryDataError x = Left $ "Pakej.Widget.Memory: " <> x
