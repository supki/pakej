{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pakej.ConfSpec (spec) where

import Control.Lens
import Data.Monoid (mempty)
import Network (PortID(..))
import Options.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Expectations.Lens

import Pakej.Conf


instance Show ParserFailure where
  show _ = "<ParserFailure>"

spec :: Spec
spec = do
  context "mode" $ do
    prop "starts as daemon if no command arguments are provided" $ \xs ->
      parse (parser "pakej.sock" xs) []
        `shouldHave` _Right.mode._Daemon

    it "converts provided options into commands" $
      parse (parser "pakej.sock" ["foo", "bar", "baz"]) ["bar"]
        `shouldPreview` "bar" `through` _Right.mode._Client

    it "does not have commands outside of options list" $
      parse (parser "pakej.sock" ["foo", "bar", "baz"]) ["xyzzy"]
        `shouldHave` _Left

  context "addr" $ do
    it "uses unix socket if no command arguments are provided" $
      parse (parser "pakej.sock" []) []
        `shouldList` [UnixSocket "pakej.sock"]
        `through` _Right.addrs.folded

    it "uses unix socket if --unix argument is provided" $
      parse (parser "pakej.sock" []) ["--unix", "nepakej.sock"]
        `shouldList` [UnixSocket "nepakej.sock"]
        `through` _Right.addrs.folded

    it "uses port number if --port argument is provided" $
      parse (parser "pakej.sock" []) ["--port", "1234"]
        `shouldList` [PortNumber 1234]
        `through` _Right.addrs.folded

    it "supports multiple socket arguments" $
      parse (parser "pakej.sock" []) ["--port", "1234", "--unix", "nepackej.sock", "--port", "5678"]
        `shouldList` [PortNumber 1234, UnixSocket "nepackej.sock", PortNumber 5678]
        `through` _Right.addrs.folded

  context "term" $ do
    it "submits to running pakej if no arguments are provided" $
      parse (parser "pakej.sock" []) []
        `shouldPreview` Submit `through` _Right.prev

    it "replaces running pakej if --replace argument is provided" $
      parse (parser "pakej.sock" []) ["--replace"]
        `shouldPreview` Replace `through` _Right.prev

    it "submits to running pakej if --submit argument is provided" $
      parse (parser "pakej.sock" []) ["--submit"]
        `shouldPreview` Submit `through` _Right.prev

  context "hostname" $ do
    it "connects to localhost if no arguments are provided" $
      parse (parser "pakej.sock" []) []
        `shouldPreview` "localhost" `through` _Right.host

    it "connects to another host if --hostname argument is provided" $
      parse (parser "pakej.sock" []) ["--hostname", "budueba.com"]
        `shouldPreview` "budueba.com" `through` _Right.host

parse :: ParserInfo a -> [String] -> Either ParserFailure a
parse = execParserPure (prefs mempty)
