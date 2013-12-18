{-# LANGUAGE OverloadedStrings #-}
module Pakej.Action
  ( -- * Type
    Pakejee, Action(..)
    -- ** Getters
  , name, action
    -- ** Constructors
  , (~>), (|>)
    -- ** Modifiers
  , delayed, separated
    -- ** Misc
  , defaultTimeout
  ) where

import Data.Text.Lazy (Text)

infix 1 ~>


data Pakejee r = Pakejee
  { _name   :: String
  , _action :: Action r
  }

data Action r =
    IO (IO r) Int
  | Group [String] r


-- | Get 'Pakejee''s name
name :: Pakejee a -> String
name = _name
{-# INLINE name #-}

-- | Get 'Pakejee''s action
action :: Pakejee r -> Action r
action = _action
{-# INLINE action #-}

-- | Construct an I/O 'Pakejee'
--
-- Default 'Action' timeout is @1@ second
(~>) :: String -> IO Text -> Pakejee Text
n ~> ioa = Pakejee { _name = n, _action = IO ioa defaultTimeout }
{-# INLINE (~>) #-}

-- | Construct an grouping 'Pakejee'
(|>) :: String -> [String] -> Pakejee Text
n |> xs = Pakejee { _name = n, _action = Group xs " | " }
{-# INLINE (|>) #-}

-- | Override timeout for I/O actions
delayed :: Int -> Pakejee a -> Pakejee a
delayed _ x@(Pakejee _ Group {})     = x
delayed t (Pakejee n (IO ior _)) = Pakejee n (IO ior t)
{-# INLINE delayed #-}

-- | Override separator for grouping actions
separated :: a -> Pakejee a -> Pakejee a
separated _ x@(Pakejee _ IO {}) = x
separated s (Pakejee n (Group ns _)) = Pakejee n (Group ns s)
{-# INLINE separated #-}

-- | Default 'Pakejee' timeout (@1@ second)
defaultTimeout :: Int
defaultTimeout = 1000000
