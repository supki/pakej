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


type Pakejee r = Access (PakejeeI r)

-- | A 'Pakejee' is a named 'Action' with an access policy
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

-- | Construct an I/O 'Pakejee'
--
-- Default 'Action' timeout is @1@ second
(~>) :: String -> IO Text -> Pakejee Text
n ~> ioa = Private PakejeeI { _name = n, _action = IO ioa defaultTimeout }

-- | Construct an grouping 'Pakejee'
(|>) :: String -> [String] -> Pakejee Text
n |> xs = Private PakejeeI { _name = n, _action = Group xs (fromString " | ") }

-- | Override timeout for I/O actions
delayed :: Int -> Pakejee a -> Pakejee a
delayed t = fmap go
 where
  go x@(PakejeeI _ Group {}) = x
  go (PakejeeI n (IO ior _)) = PakejeeI n (IO ior t)

-- | Override separator for grouping actions
separated :: a -> Pakejee a -> Pakejee a
separated sep = fmap go
 where
  go x@(PakejeeI _ IO {})      = x
  go (PakejeeI n (Group ns _)) = PakejeeI n (Group ns sep)

-- | Make 'Pakejee' private
private :: Pakejee a -> Pakejee a
private (Public (PakejeeI n a)) = Private (PakejeeI n a)
private x                       = x

-- | Make 'Pakejee' public
public :: Pakejee a -> Pakejee a
public (Private (PakejeeI n a)) = Public (PakejeeI n a)
public x                       = x

-- | Default 'Pakejee' timeout (@1@ second)
defaultTimeout :: Int
defaultTimeout = 1000000
