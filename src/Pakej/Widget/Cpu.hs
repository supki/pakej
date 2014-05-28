{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- | A parser for @\/proc\/stat@
module Pakej.Widget.Cpu
  ( widget
#ifdef TEST
  , Snapshot(..)
  , Diff(..)
  , parseSnapshot
  , parseLine
  , getData
  , cpuDataError
  , computeUsage
  , snapshotDiff
#endif
  ) where

import           Control.Category (Category(..))
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Wire (Wire)
import           Data.Foldable (asum)
import           Data.Monoid (Monoid, (<>))
import           Data.Traversable (traverse)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Read as Text
import           Prelude hiding ((.), id)
import           System.IO.Error (catchIOError)

import           Pakej.Widget (PakejWidget, Widget(..), mkFixM, mkState)

-- | Cpu usage snapshot
newtype Snapshot a = MkS [a] deriving (Show, Eq)

-- | Cpu usage snapshot difference
newtype Diff a = MkD [a]

-- | Construct a CPU usage widget
widget
  :: (Read a, Ord a, Fractional a)
  => Maybe Int     -- ^ 'Just' a core number or 'Nothing' for the total usage
  -> PakejWidget a
widget n = Widget $ usage . readSnapshot n

readSnapshot
  :: (MonadIO m, Monoid s, Ord a, Fractional a, Read a)
  => Maybe Int
  -> Wire s e m b (Snapshot a)
readSnapshot n =
  mkFixM $ \_ _ -> liftIO $ do
    d <- getData "/proc/stat"
    return $ either (\_ -> Left undefined) (maybe (Left undefined) Right . parseSnapshot n) d

handleIOError :: (IOError -> IO a) -> IO a -> IO a
handleIOError = flip catchIOError

usage :: (Monoid s, Ord a, Fractional a) => Wire s e m (Snapshot a) a
usage =
  mkState emptySnapshot $ \_dt (v, s) -> (Right (computeUsage (snapshotDiff v s)), v)

-- | Compute usage data from the difference of two cpu usage snapshots
computeUsage :: (Ord a, Fractional a) => Diff a -> a
computeUsage (MkD xs) =
  case sum xs of
    s | abs s < 0.001 -> 0
      | otherwise     -> sum (map (/ s) (take 3 xs)) * 100

-- | Compute the difference between two cpu usage snapshots
snapshotDiff :: Num a => Snapshot a -> Snapshot a -> Diff a
snapshotDiff (MkS xs) (MkS ys) = MkD (zipWith (-) xs ys)

parseSnapshot :: Fractional a => Maybe Int -> Text -> Maybe (Snapshot a)
parseSnapshot n = asum . map (parseLine n) . Text.lines

emptySnapshot :: Num a => Snapshot a
emptySnapshot = MkS (repeat 0)

parseLine :: Fractional a => Maybe Int -> Text -> Maybe (Snapshot a)
parseLine n str = MkS <$> do
  guard (("cpu" <> maybe " " (Text.pack . show) n) `Text.isPrefixOf` str)
  traverse (either (const Nothing) (Just . fst) . Text.rational) (drop 1 (Text.words str))

getData :: FilePath -> IO (Either Text Text)
getData = handleIOError (return . cpuDataError . Text.pack . show) . fmap Right . Text.readFile

cpuDataError :: Text -> Either Text a
cpuDataError x = Left $ "Pakej.Widget.Cpu: " <> x
