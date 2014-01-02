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
pakej os = do
  c <- conf (map pakejName os)
  case c of
    Left  v -> printf "pakej version %s\n" (showVersion v)
    Right x -> case view mode x of
      Client o ->
        client (view host x) (x ^?! addrs.folded) o
      Daemon ->
        daemon (view addrs x) (view prev x) os

pakejName :: Pakej r -> String
pakejName (Pakej p) = name p
