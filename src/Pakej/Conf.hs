{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Pakej configuration
module Pakej.Conf
  ( Conf(..), Mode(..), Previous(..)
  , conf
  , addrs, mode, prev, host
  , _Daemon, _Client, _Supersede, _Ignore, _Submit
#ifdef TEST
  , parser
#endif
  ) where

import Control.Lens (makeLenses, makePrisms)
import Data.Foldable (asum, foldMap)
import Data.Monoid (Monoid(..))
import Options.Applicative
import Network (PortID(..), HostName)
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>))


data Conf = Conf
  { _host  :: HostName
  , _addrs :: [PortID]
  , _mode  :: Mode
  , _prev  :: Previous
  } deriving (Show, Eq)

data Mode =
    Daemon
  | Client
    { action :: String }
    deriving (Show, Eq)

data Previous =
    Supersede
  | Ignore
  | Submit
    deriving (Show, Eq)

makeLenses ''Conf
makePrisms ''Mode
makePrisms ''Previous

conf :: [String] -> IO Conf
conf opts = do
  sock <- appDirectory "pakej" "pakej.sock"
  customExecParser (prefs showHelpOnError) (parser sock opts)

parser :: FilePath -> [String] -> ParserInfo Conf
parser sock opts = info (helper <*> go) fullDesc
 where
  go = Conf
    <$> strOption (long "hostname" <> value "localhost" <> help "hostname to connect")
    <*> asum
      [ some $ asum
        [ PortNumber . fromInteger <$> option (long "port" <> help "use network port")
        , UnixSocket <$> strOption (long "unix" <> help "use UNIX domain socket")
        ]
      , pure [UnixSocket sock]
      ]
    <*> asum
      [ subparser (foldMap clientOption opts)
      , pure Daemon
      ]
    <*> asum
      [ flag' Supersede (long "supersede" <> help "supersede running pakej")
      , flag' Submit    (long "submit"    <> help "submit to running pakej (default)")
      , pure Submit
      ]

clientOption :: String -> Mod CommandFields Mode
clientOption opt =
  command opt (info (pure (Client opt)) mempty)

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
