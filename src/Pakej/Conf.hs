{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Pakej configuration
module Pakej.Conf
  ( Conf(..), Mode(..), Previous(..)
  , conf
  , addrs, mode, prev, host
  , _Daemon, _Client, _Replace, _Ignore, _Submit
#ifdef TEST
  , parser
#endif
  ) where

import           Control.Lens (makeLenses, makePrisms)
import           Data.Foldable (asum)
import qualified Data.Text as Text
import           Data.Version (Version)
import           Options.Applicative
import           Network (PortID(..), HostName)
import           System.Directory (getAppUserDataDirectory)
import           System.FilePath ((</>))

import           Pakej.Communication (Request(..))
import           Paths_pakej (version)


data Conf = Conf
  { _host  :: HostName
  , _addrs :: [PortID]
  , _mode  :: Mode
  , _prev  :: Previous
  } deriving (Show, Eq)

data Mode =
    Daemon
  | Client
    { action :: Request }
    deriving (Show, Eq)

data Previous =
    Replace
  | Ignore
  | Submit
    deriving (Show, Eq)

makeLenses ''Conf
makePrisms ''Mode
makePrisms ''Previous

conf :: IO (Either Version Conf)
conf = appDirectory "pakej" "pakej.sock" >>= customExecParser (prefs showHelpOnError) . parser

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
      [ subparser (command "shto-to" (info (pure (Client CStatus)) fullDesc))
      , argument (Just . Client . CQuery . Text.pack) (metavar "QUERY" <> help "command to execute")
      , pure Daemon
      ]
    <*> asum
      [ flag' Replace (long "replace" <> help "replace running pakej instance")
      , flag' Submit  (long "submit"  <> help "submit to running pakej (default)")
      , pure Submit
      ]
    <* optional (switch (long "recompile" <> help "recompile pakej executable"))

  port = fmap (PortNumber . fromInteger). option
  unix = fmap UnixSocket . strOption

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
