{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Pakej configuration
module Pakej.Conf
  ( Conf(..)
  , conf
#ifdef TEST
  , _Daemon, _Client
  , parser
#endif
  ) where

import Control.Lens (makePrisms)
import Data.Foldable (asum, foldMap)
import Data.Monoid (Monoid(..))
import Options.Applicative


data Conf =
    Daemon
  | Client
    { action :: String }
    deriving (Show, Eq)

makePrisms ''Conf

conf :: [String] -> IO Conf
conf = customExecParser (prefs showHelpOnError) . parser

parser :: [String] -> ParserInfo Conf
parser opts = info (helper <*> go) fullDesc
 where
  go = asum
    [ subparser $
        foldMap clientOption opts
    , pure Daemon
    ]

clientOption :: String -> Mod CommandFields Conf
clientOption opt =
  command opt (info (pure (Client opt)) mempty)
