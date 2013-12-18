module Pakej.Conf (Conf(..), conf) where

import Data.Foldable (asum, foldMap)
import Data.Monoid (Monoid(..))
import Options.Applicative


data Conf =
    Daemon
  | Client
    { action :: String }

conf :: [String] -> IO Conf
conf opts = customExecParser (prefs showHelpOnError) (info (helper <*> parser) fullDesc)
 where
  parser = asum
    [ subparser $
        foldMap clientOption opts
    , pure Daemon
    ]

clientOption :: String -> Mod CommandFields Conf
clientOption opt =
  command opt (info (pure (Client opt)) mempty)
