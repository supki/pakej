{-# LANGUAGE DeriveDataTypeable #-}
-- | Pakej 'Widget' construction
module Pakej.Widget
  ( Widget
  , Label(..)
  , public
  , private
  , aggregate
  , text
  , system
  , widget
  , Config
  , defaultConfig
  , every
  , second
  , minute
  , inbetween
  , PakejException
  ) where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception (IOException, throwIO, handle)
import           Control.Monad (liftM)
import           Control.Monad.Trans.Writer (WriterT, tell)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Wire hiding (second, loop)
import           Data.Function (fix, on)
import           Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Text
import           Data.Traversable (Traversable, mapM)
import           Data.Typeable (Typeable, cast)
import           Prelude hiding ((.), id, mapM)
import           System.Exit (ExitCode(..))

type Widget m l v = Wire PakejException (WriterT (Map (Label l) v) m)

-- | Labal is something 'Widget''s result can be addressed by
--
-- Public results are available everywhere, but the private ones
-- are only available for local queries
data Label t =
    Public  { unLabel :: t }
  | Private { unLabel :: t }

instance Show t => Show (Label t) where
  show = show . unLabel

instance Eq t => Eq (Label t) where
  (==) = (==) `on` unLabel

instance Ord t => Ord (Label t) where
  compare = compare `on` unLabel

-- | Store the 'Widget''s result under the specified label publicly
public :: (Ord l, Monad m) => l -> Widget m l v v v
public = store . Public

-- | Store the 'Widget''s result under the specified label privately
private :: (Ord l, Monad m) => l -> Widget m l v v v
private = store . Private

-- | Store the 'Widget''s result under the specified label
store :: (Ord l, Monad m) => Label l -> Widget m l v v v
store l = mkFixM $ \_dt v -> do
  tell (Map.singleton l v)
  return (Right v)

-- | Aggregate all successful 'Widget's's results
aggregate :: (Ord l, Monad m) => [Widget m l v (Config n) Text] -> Widget m l v (Config n) Text
aggregate xs = go . (multitry xs &&& id)
 where go = mkStateM (repeat Nothing) $ \_dt ((vs, conf), s) -> do
         let s' = zipWith (<|>) vs s
             v  = Text.intercalate (separator conf) (catMaybes s')
         return (Right v, s')

multitry :: (Traversable t, Monad m) => t (Wire e m a b) -> Wire e m a (t (Maybe b))
multitry ws' = mkGen $ \dt x' -> do
  res <- mapM (\w -> stepWire w dt x') ws'
  let resx = mapM (\(mx, w) -> fmap (\x -> (x, w)) (forgive mx)) res
  return (fmap (fmap fst) resx, multitry (fmap snd res))
 where
  forgive :: Either a c -> Either b (Maybe c)
  forgive = Right . either (const Nothing) Just

-- | Exceptions that can be thrown while updating 'Widget's
data PakejException =
    PakejExitCodeException ExitCode -- ^ Widget command exited with @EXIT_FAILURE@
  | PakejIOException IOException    -- ^ Widget action has thown an 'IO' exception
  | PakejEmptyWidgetException       -- ^ Widget does not have any valid result yet
    deriving (Show, Typeable)

instance Exception PakejException where
  fromException e@(SomeException se)
    | Just e' <- fromException e = Just (PakejExitCodeException e')
    | Just e' <- fromException e = Just (PakejIOException e')
    | otherwise                 = cast se

handlePakejException :: (PakejException -> IO a) -> IO a -> IO a
handlePakejException = handle

-- | Construct a 'Widget' from the IO action returning 'Text'
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
widget s io = mkGen $ \_dt n -> do
  ref <- liftIO $ do
    ref <- newIORef Nothing
    forkIO (go (timeout n) ref s)
    return ref
  return (Left PakejEmptyWidgetException, final ref)
 where
  go n ref = fix $ \loop a -> do
    v <- handlePakejException (\_ -> return a) $ do
      v <- io a
      atomicWriteIORef ref (Just v)
      return v
    threadDelay (fromIntegral n * 1000000)
    loop v

-- | Construct a 'Widget' from a possibly empty 'IORef'
--
-- /Note/: that's basically an internal function, but maybe it'll be useful for someone
final :: (Ord l, MonadIO m) => IORef (Maybe a) -> Widget m l v x a
final ref = mkFixM $ \_dt _ ->
  liftM (maybe (Left PakejEmptyWidgetException) Right) . liftIO $ readIORef ref

-- | 'Widget' configuration
data Config n = Config
  { timeout   :: n
  , separator :: Text
  } deriving (Show, Eq)

-- | The default Pakej configuration
defaultConfig :: Num n => Config n
defaultConfig = Config { timeout = second, separator = Text.pack " | " }

-- | Wait @n@ seconds between 'Widget' updates
every :: (Ord l, Monad m) => n -> Widget m l v (Config n) (Config n)
every n = arr (\conf -> conf { timeout = n })

-- | Separate 'Widget's values by some 'Text'
inbetween :: (Ord l, Monad m) => Text -> Widget m l v (Config n) (Config n)
inbetween t = arr (\conf -> conf { separator = t })

-- | 1 second timeout
second :: Num a => a
second = 1

-- | 1 minute timeout
minute :: Num a => a
minute = 60
