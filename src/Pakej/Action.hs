{-# LANGUAGE DeriveFunctor #-}
-- | Pakej actions - 'Pakejee's
module Pakej.Action
  ( -- * Type
    Pakejee, Action(..), Access(..)
    -- ** Getters
  , name, action
    -- ** Constructors
  , (~>), (|>)
    -- ** Modifiers
  , delayed, separated, private, public
    -- ** Misc
  , defaultTimeout
  ) where

import Data.String (fromString)
import Data.Text.Lazy (Text)

infix 1 ~>, |>


-- | A 'Pakejee' is a named 'Action' with an access policy
type Pakejee r = Access (PakejeeI r)

-- | A 'PakejeeI' is a named 'Action'
data PakejeeI r = PakejeeI
  { _name   :: String
  , _action :: Action r
  }

-- | 'Public' 'Pakejee's are available to network clients
data Access a =
    Private { unAccess :: a }
  | Public  { unAccess :: a }
    deriving (Show, Eq, Functor)

-- | An 'Action' is either to do some 'IO' or to group other 'Action's
data Action r =
    IO (IO r) Int
  | Group [String] r


-- | Get 'Pakejee''s name
name :: Pakejee a -> String
name = _name . unAccess

-- | Get 'Pakejee''s action
action :: Pakejee r -> Action r
action = _action . unAccess

-- | Construct an I/O action that can be queried by the provided name
--
-- Default 'Action' timeout is @1@ second
(~>)
  :: String  -- ^ name
  -> IO Text -- ^ I/O action
  -> Pakejee Text
n ~> ioa = Private PakejeeI { _name = n, _action = IO ioa defaultTimeout }

-- | Construct a grouping action that can be queries by the provided name
(|>)
  :: String   -- ^ name
  -> [String] -- ^ names of actions to group
  -> Pakejee Text
n |> xs = Private PakejeeI { _name = n, _action = Group xs (fromString " | ") }

-- | Override the timeout for I/O action
delayed :: Int -> Pakejee a -> Pakejee a
delayed t = fmap go
 where
  go x@(PakejeeI _ Group {}) = x
  go (PakejeeI n (IO ior _)) = PakejeeI n (IO ior t)

-- | Override the separator for grouping actions
separated :: a -> Pakejee a -> Pakejee a
separated sep = fmap go
 where
  go x@(PakejeeI _ IO {})      = x
  go (PakejeeI n (Group ns _)) = PakejeeI n (Group ns sep)

-- | Private 'Pakejee's are only available locally. This means you can only
-- query those through UNIX domain sockets
--
-- This is the default
private :: Pakejee a -> Pakejee a
private (Public (PakejeeI n a)) = Private (PakejeeI n a)
private x                       = x

-- | Public 'Pakejee's are available remotely
public :: Pakejee a -> Pakejee a
public (Private (PakejeeI n a)) = Public (PakejeeI n a)
public x                       = x

-- | Default 'Pakejee' timeout (@1@ second)
defaultTimeout :: Int
defaultTimeout = 1000000
