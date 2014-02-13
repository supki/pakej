{-# LANGUAGE CPP #-}
-- | Pakej configuration
module Pakej.Conf
  ( Conf(..), Mode(..), Existing(..)
  , conf
  , addrs, mode, prev, host
  , _Daemon, _Query, _Repl, _Replace, _Respect
#ifdef TEST
  , parser
#endif
  ) where

import           Control.Lens hiding (argument)
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
  , _prev  :: Existing
  , _mode  :: Mode
  } deriving (Show, Eq)

host :: Lens' Conf HostName
host f x = f (_host x) <&> \p -> x { _host = p }
{-# INLINE host #-}

addrs :: Lens' Conf [PortID]
addrs f x = f (_addrs x) <&> \p -> x { _addrs = p }
{-# INLINE addrs #-}

prev :: Lens' Conf Existing
prev f x = f (_prev x) <&> \p -> x { _prev = p }
{-# INLINE prev #-}

mode :: Lens' Conf Mode
mode f x = f (_mode x) <&> \p -> x { _mode = p }
{-# INLINE mode #-}

data Mode =
    Daemon
  | Query Request
  | Repl
    deriving (Show, Eq)

_Daemon :: Prism' Mode ()
_Daemon = prism' (const Daemon) (\x -> case x of Daemon -> Just (); _ -> Nothing)
{-# INLINE _Daemon #-}

_Query :: Prism' Mode Request
_Query = prism' Query (\x -> case x of Query r -> Just r; _ -> Nothing)
{-# INLINE _Query #-}

_Repl :: Prism' Mode ()
_Repl = prism' (const Repl) (\x -> case x of Repl -> Just (); _ -> Nothing)
{-# INLINE _Repl #-}

data Existing =
    Replace
  | Respect
    deriving (Show, Eq)

_Replace :: Prism' Existing ()
_Replace = prism' (const Replace) (\x -> case x of Replace -> Just (); _ -> Nothing)
{-# INLINE _Replace #-}

_Respect :: Prism' Existing ()
_Respect = prism' (const Respect) (\x -> case x of Respect -> Just (); _ -> Nothing)
{-# INLINE _Respect #-}

conf :: IO (Either Version Conf)
conf = appDirectory progn (printf "%s.sock" progn) >>= withProgName progn . parse parser
 where progn   = "pakej"
       parse p = customExecParser (prefs showHelpOnError) . p

parser :: FilePath -> ParserInfo (Either Version Conf)
parser sock = info (helper <*> go) fullDesc
 where
  go = asum
    [ fmap Left versionParser
    , fmap Right confParser
    , ghostParser
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
      , flag' Respect  (long "respect"  <> help "submit to running pakej (default)")
      , pure Respect
      ]
    <*> asum
      [ flag' (Query CStatus) (long "stat" <> help "ask pakej instance what it has to show")
      , flag' Repl (long "repl" <> help "start a Repl session with the pakej instance")
      , argument (Just . Query . CQuery . Text.pack) (metavar "QUERY" <> help "query to execute")
      , pure Daemon
      ]

  ghostParser = empty
     <* asum
      [ switch (long "recompile" <> help "recompile pakej executable")
        <* many (argument Just (metavar "GHC OPTION" <> help "option to pass to GHC when recompiling"))
      , switch (long "init" <> help "initialize pakej")
      ]

  port = fmap (PortNumber . fromInteger) . option
  unix = fmap UnixSocket . strOption

-- | @\~\/.pakej\/%s@
appDirectory :: String -> FilePath -> IO FilePath
appDirectory app filename = do
  dir <- getAppUserDataDirectory app
  return (dir </> filename)
