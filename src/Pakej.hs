-- | Pakej - status bar daemon
module Pakej
  ( -- * Main function
    pakej
    -- * Actions
  , io, group
    -- * Types
  , Pakejee, PakejeeI, Action, Access
    -- ** Modifiers
  , delayed, separated, private, public
    -- ** Aliases
  , (~>), (|>)
    -- ** Misc
  , defaultTimeout
  ) where

import Control.Lens ((^?!), view, folded)
import Data.Text.Lazy (Text)

import Pakej.Action
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon


-- | Run Pakej with the provided 'Action's
pakej :: [Pakejee Text] -> IO ()
pakej os = do
  c <- conf (map name os)
  case view mode c of
    Client o ->
      client (view host c) (c ^?! addrs.folded) o
    Daemon ->
      daemon (view addrs c) (view prev c) os
