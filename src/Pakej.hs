-- | Pakej - status bar daemon
module Pakej
  ( -- * The main function
    pakej
    -- * Widget
  , Widget, (.), id
    -- ** Store the result
  , Access(..)
  , public
  , private
    -- ** Combine the results
  , aggregate
    -- ** Construct
  , text
  , system
  , widget
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
import Data.Text.Lazy (Text)
import Data.Version (showVersion)
import Prelude hiding ((.), id)
import Text.Printf (printf)

import Pakej.Widget
import Pakej.Client
import Pakej.Conf
import Pakej.Daemon


-- | Run Pakej with the provided 'Widget'
pakej :: Integral n => Widget IO Text Text (Config n) a -> IO ()
pakej ps = do
  c <- conf
  case c of
    Left  v -> printf "pakej version %s\n" (showVersion v)
    Right x -> case view mode x of
      Query q ->
        client (view host x) (x ^?! addrs.folded) q
      Repl ->
        repl (view host x) (x ^?! addrs.folded)
      Daemon ->
        daemon (view addrs x) (view prev x) ps
