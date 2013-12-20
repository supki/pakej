-- | Pakej actions - 'Pakejee's
module Pakej.Action
  ( -- * Type
    Pakejee, Action(..), Access(..)
    -- ** Getters
  , name, action, access
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


data Pakejee r = Pakejee
  { _name   :: String
  , _access :: Access
  , _action :: Action r
  }

data Access =
    Private
  | Public
    deriving (Show, Eq)

data Action r =
    IO (IO r) Int
  | Group [String] r


-- | Get 'Pakejee''s name
name :: Pakejee a -> String
name = _name

-- | Get 'Pakejee''s action
action :: Pakejee r -> Action r
action = _action

-- | Get 'Pakejee''s access
access :: Pakejee a -> Access
access = _access

-- | Construct an I/O 'Pakejee'
--
-- Default 'Action' timeout is @1@ second
(~>) :: String -> IO Text -> Pakejee Text
n ~> ioa = Pakejee { _name = n, _access = Private, _action = IO ioa defaultTimeout }

-- | Construct an grouping 'Pakejee'
(|>) :: String -> [String] -> Pakejee Text
n |> xs = Pakejee { _name = n, _access = Private, _action = Group xs (fromString " | ") }

-- | Override timeout for I/O actions
delayed :: Int -> Pakejee a -> Pakejee a
delayed _ x@(Pakejee _ _ Group {}) = x
delayed t (Pakejee n a (IO ior _)) = Pakejee n a (IO ior t)

-- | Override separator for grouping actions
separated :: a -> Pakejee a -> Pakejee a
separated _ x@(Pakejee _ _ IO {})      = x
separated s (Pakejee n a (Group ns _)) = Pakejee n a (Group ns s)

-- | Make 'Pakejee' private
private :: Pakejee a -> Pakejee a
private ~(Pakejee n _ a) = Pakejee n Private a

-- | Make 'Pakejee' public
public :: Pakejee a -> Pakejee a
public ~(Pakejee n _ a) = Pakejee n Public a

-- | Default 'Pakejee' timeout (@1@ second)
defaultTimeout :: Int
defaultTimeout = 1000000
