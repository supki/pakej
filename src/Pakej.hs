-- | Pakej - status bar daemon
module Pakej
  ( pakej
  , Pakejee, (~>), (|>), delayed, separated, private, public, defaultTimeout
  ) where

import Control.Lens ((^?!), view, folded)
import Data.Text.Lazy (Text)

import Pakej.Action
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon


-- | Run Pakej with the provided options
pakej :: [Pakejee Text] -> IO ()
pakej os = do
  c <- conf (map name os)
  case view mode c of
    Client o ->
      client (view host c) (c ^?! addrs.folded) o
    Daemon ->
      daemon (view addrs c) (view prev c) os
