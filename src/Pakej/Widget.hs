{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Pakej 'Widget's
module Pakej.Widget
  ( Widget(..)
  , fromWire
    -- * Store the result
  , Access(..)
  , public
  , private
    -- * Combine the results
  , aggregate
    -- * Construct
  , text
  , system
  , constant
  , widget
    -- * Configure
  , Config
  , defaultConfig
  , every
  , second
  , minute
  , hour
  , day
  , inbetween
    -- * Misc
  , PakejException
    -- * netwire-4 helpers
  , mkFix
  , mkFixM
  , mkState
  , mkStateM
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (Exception(..), SomeException(..), throwIO, handle)
import           Control.Monad (liftM)
import           Control.Monad.Trans.Writer (WriterT, tell)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Wire hiding (second, loop)
import           Data.Function (fix)
import           Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Traversable (Traversable, mapM)
import           Data.Typeable (Typeable)
import           Prelude hiding ((.), id, mapM)
import           System.Exit (ExitCode(..))

{-# ANN module "HLint: ignore Use second" #-}


-- | Widget is an Automaton that operates over 'Monad' @m@ collecting
-- results in the mapping @l -> v@
newtype Widget m l v a b = Widget
 { unWidget :: Wire (Timed NominalDiffTime ()) SomeException (WriterT (Endo (Map l (Access v))) m) a b
 } deriving (Category, Functor, Applicative)

{-# ANN fromWire "HLint: ignore Eta reduce" #-}
-- | Get a widget from an abstract 'Wire'
fromWire :: Monad n => (forall e m. Monad m => Wire (Timed NominalDiffTime ()) e m a b) -> Widget n l v a b
fromWire w = Widget w

-- | Public results are available everywhere, but the private ones are only available
-- for local queries (meaning queries to the local UNIX socket Pakej's listening)
data Access t =
    Public  { unAccess :: t }
  | Private { unAccess :: t }
    deriving (Show, Eq)

-- | Store the 'Widget''s result under the specified label publicly
public :: (Ord l, Monad m) => l -> Widget m l v v v
public = store Public

-- | Store the 'Widget''s result under the specified label privately
private :: (Ord l, Monad m) => l -> Widget m l v v v
private = store Private

-- | Store the 'Widget''s result under the specified label
store :: (Ord l, Monad m) => (v -> Access v) -> l -> Widget m l v v v
store f l = Widget . mkFixM $ \_dt v -> do
  tell (Endo (Map.insert l (f v)))
  return (Right v)

-- | Aggregate all successful 'Widget's' results
--
-- Failed 'Widget's' results are skipped so they do not clutter the resulting value
aggregate :: (Ord l, Monad m) => [Widget m l v (Config n) Text] -> Widget m l v (Config n) Text
aggregate xs = go . Widget (dispatch (map unWidget xs) &&& id)
 where go = Widget . mkStateM (repeat Nothing) $ \_dt ((vs, conf), s) -> do
         let s' = zipWith (<|>) vs s
             v  = Text.intercalate (separator conf) (catMaybes s')
         return (Right v, s')

-- | Step through wires, mapping each success @x@ to @Just x@ and each inhibition @y@ to @Nothing@
dispatch :: (Traversable t, Monad m, Monoid s) => t (Wire s e m a b) -> Wire s e m a (t (Maybe b))
dispatch ws = mkGen $ \dt x -> do
  t <- stepWires dt x ws
  return (values t, dispatch (wires t))
 where
  values = pure . fmap (either (const Nothing) Just . fst)
  wires  = fmap snd

-- | Step through wires using the same time delta and the same input for all of them,
-- collecting their results and updated wires
stepWires
  :: (Traversable t, Monad m)
  => s -> a -> t (Wire s e m a b) -> m (t (Either e b, Wire s e m a b))
stepWires dt x = mapM (\w -> stepWire w dt (Right x))

-- | Exceptions that can be thrown while updating 'Widget's
data PakejException =
    EmptyWidgetException -- ^ Widget does not have any valid result yet
    deriving (Show, Typeable)

instance Exception PakejException

-- | Construct a 'Widget' from the 'IO' action that returns 'Text'
text :: (Ord l, MonadIO m, Integral n) => IO Text -> Widget m l v (Config n) Text
text = constant

-- | Construct a 'Widget' from the external command.
system
  :: (Ord l, MonadIO m, Integral n)
  => IO (ExitCode, Text, Text) -> Widget m l v (Config n) Text
system io = constant $ do
  (ec, out, _) <- io
  case ec of
    ExitSuccess -> return out
    _           -> throwIO ec

-- | Construct a 'Widget' from the IO action
constant :: (Ord l, MonadIO m, Integral n) => IO b -> Widget m l v (Config n) b
constant io = widget undefined (const io)

-- | Construct a 'Widget' from the /iterating/ IO action
widget :: (Ord l, MonadIO m, Integral n) => a -> (a -> IO a) -> Widget m l v (Config n) a
widget s io = Widget . mkGen $ \_dt n -> do
  ref <- liftIO $ do
    ref <- newIORef Nothing
    forkIO (go (timeout n) ref s)
    return ref
  return (Left (SomeException EmptyWidgetException), unWidget (final ref))
 where
  go n ref = fix $ \loop a -> do
    v <- handle (\(SomeException _) -> return a) $ do
      v <- io a
      atomicWriteIORef ref (Just v)
      return v
    threadDelay (fromIntegral n * 1000000)
    loop v

-- | Construct a 'Widget' from a possibly empty 'IORef'
--
-- /Note/: that's basically an internal function, but maybe it'll be useful for someone
final :: (Ord l, MonadIO m) => IORef (Maybe a) -> Widget m l v x a
final ref = Widget . mkFixM $ \_dt _ -> liftIO $
  liftM (maybe (Left (SomeException EmptyWidgetException)) Right) (readIORef ref)

-- | 'Widget' configuration
data Config n = Config
  { timeout   :: n    -- ^ Time to wait between 'Widget' updates
  , separator :: Text -- ^ Text to separate aggregated results
  } deriving (Show, Eq)

-- | The default 'Widget' configuration
defaultConfig :: Num n => Config n
defaultConfig = Config { timeout = second, separator = Text.pack " | " }

-- | Wait @n@ seconds between 'Widget' updates
every :: (Ord l, Monad m) => n -> Widget m l v (Config n) (Config n)
every n = Widget $ arr (\conf -> conf { timeout = n })

-- | Separate 'Widget's values by some 'Text'
inbetween :: (Ord l, Monad m) => Text -> Widget m l v (Config n) (Config n)
inbetween t = Widget $ arr (\conf -> conf { separator = t })

-- | 1 second timeout
second :: Num a => a
second = 1

-- | 1 minute timeout
minute :: Num a => a
minute = 60 * second

-- | 1 hour timeout
hour :: Num a => a
hour = 60 * minute

-- | 1 day timeout
day :: Num a => a
day = 24 * hour

-- | A port of netwire-4's @mkFix@
mkFix :: Monoid s => (s -> a -> Either e b) -> Wire s e m a b
mkFix f = let w = mkPure (\dt -> (\x -> (x, w)) . f dt) in w

-- | A port of netwire-4's @mkFixM@
mkFixM :: (Monad m, Monoid s) => (s -> a -> m (Either e b)) -> Wire s e m a b
mkFixM f = let w = mkGen (\dt -> liftM (\x -> (x, w)) . f dt) in w

-- | A port of netwire-4's @mkState@
mkState :: Monoid t => s -> (t -> (a, s) -> (Either e b, s)) -> Wire t e m a b
mkState s0 f = loop s0
 where
  loop s' = mkPure $ \dt x' -> (\(a, b) -> (a, loop b)) (f dt (x', s'))

-- | A port of netwire-4's @mkStateM@
mkStateM :: (Monad m, Monoid t) => s -> (t -> (a, s) -> m (Either e b, s)) -> Wire t e m a b
mkStateM s0 f = loop s0
 where
  loop s' = mkGen $ \dt x' -> liftM (\(a, b) -> (a, loop b)) (f dt (x', s'))
