{-# LANGUAGE DeriveFunctor #-}
-- | Pakej actions - 'Pakejee's
module Pakej.Action
  ( -- * Actions
    io, group
    -- * Types
  , Pakejee, PakejeeI, Action(..), name, action
  , Access(..), access
    -- ** Modifiers
  , delayed, separated, private, public
    -- ** Aliases
  , (~>), (|>)
    -- ** Misc
  , defaultTimeout
  ) where

import Data.Text.Lazy (Text, pack)

infix 1 ~>, |>

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XStandaloneDeriving
-- >>> instance Show (IO a) where show _ = "<IO action>"
-- >>> deriving instance Show r => Show (Action r)
-- >>> deriving instance Show r => Show (PakejeeI r)


-- | A 'Pakejee' is a named 'Action' with an 'Access' policy
type Pakejee r = Access (PakejeeI r)

-- | A 'PakejeeI' is a named 'Action'
data PakejeeI r = PakejeeI String (Action r)
  deriving (Functor)

_name :: PakejeeI r -> String
_name (PakejeeI n _) = n

_action :: PakejeeI r -> Action r
_action (PakejeeI _ a) = a

-- | Get 'Pakejee''s name
name :: Pakejee a -> String
name = _name . access

-- | Get 'Pakejee''s action
action :: Pakejee r -> Action r
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
data Action r =
    IO (IO r) Int
  | Group [String] r
    deriving (Functor)


-- | Construct an I/O action that can be queried by the provided name
--
-- Default 'Access' policy is 'Private'
--
-- Default timeout is @1@ second
--
-- >>> io "timestamp" (return "1388418907")
-- Private (PakejeeI "timestamp" (IO <IO action> 1000000))
io
  :: String  -- ^ name
  -> IO Text -- ^ I/O action
  -> Pakejee Text
io n ioa = Private (PakejeeI n (IO ioa defaultTimeout))

-- | An infix alias for 'io'
(~>) :: String -> IO Text -> Pakejee Text
(~>) = io

-- | Construct an action that groups results of provided queries
-- names with a separator
--
-- Default 'Access' policy is 'Private'
--
-- Default separator is @ | @
--
-- >>> group "group" ["foo", "bar", "baz"]
-- Private (PakejeeI "group" (Group ["foo","bar","baz"] " | "))
group
  :: String   -- ^ name
  -> [String] -- ^ names of actions to group
  -> Pakejee Text
group n xs = Private (PakejeeI n (Group xs (pack " | ")))

-- | An infix alias for 'group'
(|>) :: String -> [String] -> Pakejee Text
(|>) = group

-- | Override the timeout for the I/O 'Action'
--
-- >>> delayed (3 * defaultTimeout) $ io "timestamp" (return "1388418907")
-- Private (PakejeeI "timestamp" (IO <IO action> 3000000))
delayed :: Int -> Pakejee a -> Pakejee a
delayed t = fmap go
 where
  go x@(PakejeeI _ Group {}) = x
  go (PakejeeI n (IO ior _)) = PakejeeI n (IO ior t)

-- | Override the separator for the grouping 'Action'
--
-- >>> separated " % "  $ group "group" ["foo", "bar", "baz"]
-- Private (PakejeeI "group" (Group ["foo","bar","baz"] " % "))
separated :: a -> Pakejee a -> Pakejee a
separated sep = fmap go
 where
  go x@(PakejeeI _ IO {})      = x
  go (PakejeeI n (Group ns _)) = PakejeeI n (Group ns sep)

-- | Public 'Pakejee's are available remotely
--
-- >>> public $ group "group" ["foo", "bar", "baz"]
-- Public (PakejeeI "group" (Group ["foo","bar","baz"] " | "))
public :: Pakejee a -> Pakejee a
public (Private (PakejeeI n a)) = Public (PakejeeI n a)
public x                       = x

-- | Private 'Pakejee's are only available locally. This means you can only
-- query those through UNIX domain sockets
--
-- This is the default
--
-- >>> group "group" ["foo", "bar", "baz"]
-- Private (PakejeeI "group" (Group ["foo","bar","baz"] " | "))
--
-- >>> private $ public $ group "group" ["foo", "bar", "baz"]
-- Private (PakejeeI "group" (Group ["foo","bar","baz"] " | "))
private :: Pakejee a -> Pakejee a
private (Public (PakejeeI n a)) = Private (PakejeeI n a)
private x                       = x

-- | Default 'Pakejee' timeout (@1@ second)
defaultTimeout :: Int
defaultTimeout = 1000000
