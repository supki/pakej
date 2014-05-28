{-# LANGUAGE LambdaCase #-}
-- | Pakej - status bar daemon
module Pakej
  ( -- * The main function
    pakej
  , pakejWith
    -- * Widget
  , PakejWidget
  , Widget
  , (.)
  , id
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
  , fromWire
    -- ** Configure
    -- *** Pakej Configuration
  , PakejConf
  , defaultPakejConf
  , addrs
  , PortID(..)
  , policy
  , Policy(..)
    -- *** Widget Configuration
  , WidgetConf
  , defaultWidgetConf
  , every
  , second
  , minute
  , hour
  , day
  , inbetween
    -- * netwire-4 helpers
  , mkFix
  , mkFixM
  , mkState
  , mkStateM
  ) where

import Control.Category ((.), id)
import Control.Lens
import Control.Monad
import Data.Version (showVersion)
import Network (PortID(..))
import Prelude hiding ((.), id)
import System.FilePath ((</>), isRelative)
import System.Directory (getAppUserDataDirectory)
import Text.Printf (printf)

import Pakej.Client
import Pakej.Conf
import Pakej.Daemon
import Pakej.Widget

-- | Run Pakej with the provided 'Widget'
pakej :: PakejWidget a -> IO ()
pakej = pakejWith defaultPakejConf

-- | Run Pakej with the customized 'PakejConf' and the provided 'Widget'
pakejWith :: PakejConf -> PakejWidget a -> IO ()
pakejWith dc ps =
  options dc >>= either
    (printf "pakej version %s\n" . showVersion)
    (fixRelativeUnixSocketPaths >=> \conf ->
      case view mode conf of
        Query q -> oneshot conf q
        Repl    -> repl conf
        Daemon  -> daemon conf ps)

fixRelativeUnixSocketPaths :: PakejConf -> IO PakejConf
fixRelativeUnixSocketPaths = (addrs.traverse._UnixSocket.filtered isRelative) pakejDirectory

_UnixSocket :: Prism' PortID String
_UnixSocket = prism' UnixSocket (\case UnixSocket x -> Just x; _ -> Nothing)

pakejDirectory :: FilePath -> IO FilePath
pakejDirectory path = getAppUserDataDirectory "pakej" <&> (</> path)
