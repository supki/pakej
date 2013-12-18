module Pakej
  ( pakej
  , Pakejee, (~>), (|>), delayed, separated, defaultTimeout
  ) where

import Pakej.Action
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon

import Data.Text.Lazy (Text)


pakej :: [Pakejee Text] -> IO ()
pakej os = do
  c <- conf (map name os)
  case c of
    Client o  -> client o
    Daemon    -> daemon os
