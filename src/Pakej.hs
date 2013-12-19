module Pakej
  ( pakej
  , Pakejee, (~>), (|>), delayed, separated, defaultTimeout
  ) where

import Control.Lens (view)
import Data.Text.Lazy (Text)

import Pakej.Action
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon


pakej :: [Pakejee Text] -> IO ()
pakej os = do
  c <- conf (map name os)
  case view mode c of
    Client o  -> client (view host c) (view addr c) o
    Daemon    -> daemon (view addr c) (view prev c) os
