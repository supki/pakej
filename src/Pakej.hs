-- | Pakej - status bar daemon
module Pakej
  ( -- * The main function
    pakej
    -- * Widget
  , PakejWidget, Widget, (.), id
    -- ** Store the result
  , Access(..)
  , public
  , private
    -- ** Combine the results
  , aggregate
    -- ** Construct
  , text
  , system
  , constant
  , widget
  , query
    -- ** Configure
  , Config
  , defaultConfig
  , every
  , second
  , minute
  , inbetween
    -- ** Misc
  , PakejException
  ) where

import Control.Category ((.), id)
import Control.Lens ((^?!), view, folded)
import Data.Version (showVersion)
import Prelude hiding ((.), id)
import Text.Printf (printf)

import Pakej.Client
import Pakej.Conf
import Pakej.Daemon
import Pakej.Widget


-- | Run Pakej with the provided 'Widget'
pakej :: PakejWidget a -> IO ()
pakej ps = do
  c <- conf
  case c of
    Left  v -> printf "pakej version %s\n" (showVersion v)
    Right x -> case view mode x of
      Query q ->
        oneshot (view host x) (x ^?! addrs.folded) q
      Repl ->
        repl (view host x) (x ^?! addrs.folded)
      Daemon ->
        daemon x ps
