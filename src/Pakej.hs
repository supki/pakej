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
import Data.Version (showVersion)
import Text.Printf (printf)

import Pakej.Action
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon


-- | Run Pakej with the provided 'Action's
pakej :: [Pakej Text] -> IO ()
pakej ps = do
  c <- conf
  case c of
    Left  v -> printf "pakej version %s\n" (showVersion v)
    Right x -> case view mode x of
      Client q ->
        client (view host x) (x ^?! addrs.folded) q
      Daemon ->
        daemon (view addrs x) (view prev x) ps
