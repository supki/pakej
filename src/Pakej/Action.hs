{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
-- | Pakej actions - 'Pakej's
module Pakej.Action
  ( -- * Actions
    run, io, group
    -- * Types
  , Pakej(..), Pakejee, Named, Action(..), name, action
  , Access(..), access
    -- ** Modifiers
  , delay, separate, private, public
    -- ** Aliases
  , (~>), (|>)
    -- ** Misc
  , IO, Group, defaultTimeout
  ) where

import Data.Text.Lazy (Text, pack)

infix 1 ~>, |>

-- $setup
-- >>> :set -XGADTs
-- >>> :set -XOverloadedStrings
-- >>> :set -XStandaloneDeriving
-- >>> instance Show (IO a) where show _ = "<IO action>"
-- >>> deriving instance Show r => Show (Action m r)


-- | A 'Pakej' is a 'Pakejee' with the erased 'Action' tag
data Pakej r where
  Pakej :: Pakejee m r -> Pakej r

-- | Wrap a 'Pakejee' into 'Pakej'
run :: Pakejee m r -> Pakej r
run = Pakej

-- | A 'Pakejee' is a named 'Action' with an 'Access' policy
type Pakejee m r = Access (Named (Action m r))

-- | 'Named' things are things with a 'String' attached to them
data Named f = Named String f
  deriving (Show, Eq, Functor)

_name :: Named a -> String
_name (Named n _) = n

_action :: Named a -> a
_action (Named _ a) = a

-- | Get 'Pakej''s name
name :: Pakejee m r -> String
name = _name . access

-- | Get 'Pakejee''s action
action :: Pakejee m r -> Action m r
action = _action . access

-- | 'Access' policy for 'Pakejee'
--
-- 'Private' (which is the default) 'Pakejee's
-- are available only to local clients
--
-- 'Public' 'Pakejee's are available to remote as
-- well as local clients
data Access a =
    Private a
  | Public  a
    deriving (Show, Eq, Functor)

-- | Get a thing inside access policy
access :: Access a -> a
access (Private a) = a
access (Public  a) = a

-- | An 'Action' either does some 'IO' or groups other 'Action's
data Action (m :: * -> *) r where
  IO    :: IO r -> Int -> Action IO r
  Group :: [String] -> r -> Action Group r

instance Functor (Action f) where
  fmap f (IO ioa n)   = IO (fmap f ioa) n
  fmap f (Group xs a) = Group xs (f a)

-- | A tag for grouping 'Action's
data Group a


-- | Construct an I/O action that can be queried by the provided name
--
-- Default 'Access' policy is 'Private'
--
-- Default timeout is @1@ second
--
-- >>> io "timestamp" (return "1388418907")
-- Private (Named "timestamp" (IO <IO action> 1000000))
io
  :: String  -- ^ name
  -> IO Text -- ^ I/O action
  -> Pakejee IO Text
io n ioa = Private (Named n (IO ioa defaultTimeout))

-- | An infix alias for 'io'
(~>) :: String -> IO Text -> Pakejee IO Text
(~>) = io

-- | Construct an action that groups results of provided queries
-- names with a separator
--
-- Default 'Access' policy is 'Private'
--
-- Default separator is @ | @
--
-- >>> group "group" ["foo", "bar", "baz"]
-- Private (Named "group" (Group ["foo","bar","baz"] " | "))
group
  :: String   -- ^ name
  -> [String] -- ^ names of actions to group
  -> Pakejee Group Text
group n xs = Private (Named n (Group xs (pack " | ")))

-- | An infix alias for 'group'
(|>) :: String -> [String] -> Pakejee Group Text
(|>) = group

-- | Change timeout for the I/O 'Action'
--
-- >>> delay (* 3) $ io "timestamp" (return "1388418907")
-- Private (Named "timestamp" (IO <IO action> 3000000))
delay :: (Int -> Int) -> Pakejee IO a -> Pakejee IO a
delay f = fmap $ \(Named n (IO ior t)) -> Named n (IO ior (f t))

-- | Override the separator for the grouping 'Action'
--
-- >>> separate " % "  $ group "group" ["foo", "bar", "baz"]
-- Private (Named "group" (Group ["foo","bar","baz"] " % "))
separate :: a -> Pakejee Group a -> Pakejee Group a
separate sep = fmap $ \(Named n (Group ns _)) -> Named n (Group ns sep)

-- | Public 'Pakejee's are available remotely
--
-- >>> public $ group "group" ["foo", "bar", "baz"]
-- Public (Named "group" (Group ["foo","bar","baz"] " | "))
public :: Pakejee m a -> Pakejee m a
public (Private (Named n a)) = Public (Named n a)
public x                     = x

-- | Private 'Pakejee's are only available locally. This means you can only
-- query those through UNIX domain sockets
--
-- This is the default
--
-- >>> group "group" ["foo", "bar", "baz"]
-- Private (Named "group" (Group ["foo","bar","baz"] " | "))
--
-- >>> private $ public $ group "group" ["foo", "bar", "baz"]
-- Private (Named "group" (Group ["foo","bar","baz"] " | "))
private :: Pakejee m a -> Pakejee m a
private (Public (Named n a)) = Private (Named n a)
private x                    = x

-- | Default 'Pakejee' timeout (@1@ second)
defaultTimeout :: Int
defaultTimeout = 1000000
