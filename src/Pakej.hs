-- | Pakej - status bar daemon
module Pakej
  ( -- * Main function
    pakej
    -- * Actions
  , run, io, group
    -- * Types
  , Pakej, Pakejee, Named, Action, Access
    -- ** Modifiers
  , delay, separate, private, public
    -- ** Aliases
  , (~>), (|>)
    -- ** Misc
  , IO, Group, defaultTimeout
  ) where

import Control.Lens ((^?!), view, folded)
import Data.Text.Lazy (Text)

import Pakej.Action
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon


-- | Run Pakej with the provided 'Action's
pakej :: [Pakej Text] -> IO ()
pakej os = do
  c <- conf (map pakejName os)
  case view mode c of
    Client o ->
      client (view host c) (c ^?! addrs.folded) o
    Daemon ->
      daemon (view addrs c) (view prev c) os

pakejName :: Pakej r -> String
pakejName (Pakej p) = name p
