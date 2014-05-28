{-# LANGUAGE CPP #-}
-- | Pakej configuration
module Pakej.Conf
  ( PakejConf(..), Mode(..), Policy(..)
  , options
  , defaultPakejConf
  , addrs, mode, policy, host, foreground
  , _Daemon, _Query, _Repl, _Replace, _Respect
#ifdef TEST
  , parser
#endif
  ) where

import           Control.Lens hiding (argument)
import           Data.Foldable (asum)
import qualified Data.Text as Text
import           Data.Version (Version)
import           Options.Applicative hiding ((&))
import           Network (PortID(..), HostName)
import           System.Environment (withProgName)

import           Pakej.Protocol (Request(..))
import           Paths_pakej (version)


data PakejConf = PakejConf
  { _host       :: HostName
  , _addrs      :: [PortID]
  , _policy     :: Policy
  , _mode       :: Mode
  , _foreground :: Bool
  } deriving (Show, Eq)

host :: Lens' PakejConf HostName
host f x = f (_host x) <&> \p -> x { _host = p }
{-# INLINE host #-}

addrs :: Lens' PakejConf [PortID]
addrs f x = f (_addrs x) <&> \p -> x { _addrs = p }
{-# INLINE addrs #-}

policy :: Lens' PakejConf Policy
policy f x = f (_policy x) <&> \p -> x { _policy = p }
{-# INLINE policy #-}

mode :: Lens' PakejConf Mode
mode f x = f (_mode x) <&> \p -> x { _mode = p }
{-# INLINE mode #-}

foreground :: Lens' PakejConf Bool
foreground f x = f (_foreground x) <&> \p -> x { _foreground = p }
{-# INLINE foreground #-}

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

data Policy =
    Replace
  | Respect
    deriving (Show, Eq)

_Replace :: Prism' Policy ()
_Replace = prism' (const Replace) (\x -> case x of Replace -> Just (); _ -> Nothing)
{-# INLINE _Replace #-}

_Respect :: Prism' Policy ()
_Respect = prism' (const Respect) (\x -> case x of Respect -> Just (); _ -> Nothing)
{-# INLINE _Respect #-}

options :: PakejConf -> IO (Either Version PakejConf)
options = withProgName "pakej" . customExecParser (prefs showHelpOnError) . parser

parser :: PakejConf -> ParserInfo (Either Version PakejConf)
parser conf = info (helper <*> go) fullDesc
 where
  go = asum
    [ fmap Left versionParser
    , fmap Right confParser
    , ghostParser
    ]

  versionParser = flag' version (long "version" <> short 'v' <> help "print version information")

  confParser = withDefaultConf conf
    <$> optional (strOption (long "hostname" <> help "hostname to connect"))
    <*> optional (some (asum
        [ port (long "port" <> help "port to connect")
        , unix (long "unix" <> help "UNIX domain socket to connect")
        ]))
    <*> optional (asum
      [ flag' Replace (long "replace" <> help "replace running pakej instance")
      , flag' Respect  (long "respect"  <> help "submit to running pakej (default)")
      ])
    <*> optional (asum
      [ flag' (Query CStatus) (long "stat" <> help "ask pakej instance what it has to show")
      , flag' Repl (long "repl" <> help "start a Repl session with the pakej instance")
      , argument (Just . Query . CQuery . Text.pack) (metavar "QUERY" <> help "query to execute")
      ])
    <*> switch (long "foreground" <> short 'f' <> help "stay in the foreground, don't daemonize")

  ghostParser = empty
     <* asum
      [ switch (long "recompile" <> help "recompile pakej executable")
        <* many (argument Just (metavar "GHC OPTION" <> help "option to pass to GHC when recompiling"))
      , switch (long "init" <> help "initialize pakej")
      , switch (long "edit" <> help "edit pakej.hs, recompile on changes")
        <* many (argument Just (metavar "GHC OPTION" <> help "option to pass to GHC when recompiling"))
      ]

  port = fmap (PortNumber . fromInteger) . option
  unix = fmap UnixSocket . strOption

withDefaultConf
  :: PakejConf -> Maybe HostName -> Maybe [PortID] -> Maybe Policy -> Maybe Mode -> Bool -> PakejConf
withDefaultConf c mhm mpid mp mm f = c
  & maybe id (set host) mhm
  & maybe id (set addrs) mpid
  & maybe id (set policy) mp
  & maybe id (set mode) mm
  & set foreground f

defaultPakejConf :: PakejConf
defaultPakejConf = PakejConf
  { _host       = "localhost"
  , _addrs      = [UnixSocket "pakej.sock"]
  , _policy     = Respect
  , _mode       = Daemon
  , _foreground = False
  }
