{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Pakej configuration
module Pakej.Conf
  ( Conf(..), Mode(..), Previous(..)
  , conf
  , addrs, mode, prev, host
  , _Daemon, _Client, _Replace, _Submit
#ifdef TEST
  , parser
#endif
  ) where

import           Control.Lens (makeLenses, makePrisms)
import           Data.Foldable (asum)
import qualified Data.Text.Lazy as Text
import           Data.Version (Version)
import           Options.Applicative
import           Network (PortID(..), HostName)
import           System.Directory (getAppUserDataDirectory)
import           System.Environment (withProgName)
import           System.FilePath ((</>))
import           Text.Printf (printf)

import           Pakej.Communication (Request(..))
import           Paths_pakej (version)


data Conf = Conf
  { _host  :: HostName
  , _addrs :: [PortID]
  , _prev  :: Previous
  , _mode  :: Mode
  } deriving (Show, Eq)

data Mode =
    Daemon
  | Client Request
    deriving (Show, Eq)

data Previous =
    Replace
  | Submit
    deriving (Show, Eq)

makeLenses ''Conf
makePrisms ''Mode
makePrisms ''Previous

conf :: IO (Either Version Conf)
conf = appDirectory progn (printf "%s.sock" progn) >>= withProgName progn . parse parser
 where
  progn   = "pakej"
  parse p = customExecParser (prefs showHelpOnError) . p

parser :: FilePath -> ParserInfo (Either Version Conf)
parser sock = info (helper <*> go) fullDesc
 where
  go = asum
    [ fmap Left versionParser
    , fmap Right confParser
    ]

  versionParser = flag' version (long "version" <> short 'v' <> help "print version information")

  confParser = Conf
    <$> strOption (long "hostname" <> value "localhost" <> help "hostname to connect")
    <*> asum
      [ some $ asum
        [ port (long "port" <> help "port to connect")
        , unix (long "unix" <> help "UNIX domain socket to connect")
        ]
      , pure [UnixSocket sock]
      ]
    <*> asum
      [ flag' Replace (long "replace" <> help "replace running pakej instance")
      , flag' Submit  (long "submit"  <> help "submit to running pakej (default)")
      , pure Submit
      ]
    <* optional (switch (long "recompile" <> help "recompile pakej executable"))
    <*> asum
      [ flag' (Client CStatus) (long "stat" <> help "ask pakej instance what it has to show")
      , argument (Just . Client . CQuery . Text.pack) (metavar "QUERY" <> help "query to execute")
      , pure Daemon
      ]

  port = fmap (PortNumber . fromInteger). option
  unix = fmap UnixSocket . strOption

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
