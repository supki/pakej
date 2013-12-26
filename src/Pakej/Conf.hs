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
import           Data.Foldable (asum, foldMap)
import           Data.Monoid (Monoid(..))
import qualified Data.Text as Text
import           Options.Applicative
import           Network (PortID(..), HostName)
import           System.Directory (getAppUserDataDirectory)
import           System.FilePath ((</>))

import           Pakej.Communication (Client(..))


data Conf = Conf
  { _host  :: HostName
  , _addrs :: [PortID]
  , _mode  :: Mode
  , _prev  :: Previous
  } deriving (Show, Eq)

data Mode =
    Daemon
  | Client
    { action :: Client }
    deriving (Show, Eq)

data Previous =
    Replace
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
        [ port (long "port" <> help "use network port")
        , unix (long "unix" <> help "use UNIX domain socket")
        ]
      , pure [UnixSocket sock]
      ]
    <*> asum
      [ subparser
        (  command "shto-to" (info (pure (Client CStatus)) mempty)
        <> foldMap clientOption opts
        )
      , pure Daemon
      ]
    <*> asum
      [ flag' Replace (long "replace" <> help "replace running pakej (if any)")
      , flag' Submit  (long "submit"  <> help "submit to running pakej (default)")
      , pure Submit
      ]
    <* optional (switch (long "recompile" <> help "recompile pakej executable"))

  port = fmap (PortNumber . fromInteger). option
  unix = fmap UnixSocket . strOption

clientOption :: String -> Mod CommandFields Mode
clientOption opt =
  command opt (info (pure (Client (CQuery (Text.pack opt)))) mempty)

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
