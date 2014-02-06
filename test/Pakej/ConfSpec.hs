{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pakej.ConfSpec (spec) where

import Control.Lens
import Data.Monoid (mempty)
import Data.Version (showVersion)
import Network (PortID(..))
import Options.Applicative
import Test.Hspec
import Test.Hspec.Expectations.Lens

import Pakej.Communication
import Pakej.Conf


instance Show ParserFailure where
  show _ = "<ParserFailure>"

spec :: Spec
spec = do
  context "version" $ do
    it "shows verion if --version is provided" $
      parse (parser "pakej.sock") ["--version"]
        `shouldPreview` "0.1.0.0" `through` _Right._Left.to showVersion

    it "shows verion if -v is provided" $
      parse (parser "pakej.sock") ["-v"]
        `shouldPreview` "0.1.0.0" `through` _Right._Left.to showVersion

  context "mode" $ do
    it "starts as daemon if no command arguments are provided" $
      parse (parser "pakej.sock") []
        `shouldHave` _Right._Right.mode._Daemon

    it "converts commands into queries" $
      parse (parser "pakej.sock") ["bar"]
        `shouldPreview` CQuery "bar" `through` _Right._Right.mode._Client

    it "converts --stat option into a status query" $
      parse (parser "pakej.sock") ["--stat"]
        `shouldPreview` CStatus `through` _Right._Right.mode._Client

  context "addr" $ do
    it "uses unix socket if no command arguments are provided" $
      parse (parser "pakej.sock") []
        `shouldList` [UnixSocket "pakej.sock"]
        `through` _Right._Right.addrs.folded

    it "uses unix socket if --unix argument is provided" $
      parse (parser "pakej.sock") ["--unix", "nepakej.sock"]
        `shouldList` [UnixSocket "nepakej.sock"]
        `through` _Right._Right.addrs.folded

    it "uses port number if --port argument is provided" $
      parse (parser "pakej.sock") ["--port", "1234"]
        `shouldList` [PortNumber 1234]
        `through` _Right._Right.addrs.folded

    it "supports multiple socket arguments" $
      parse (parser "pakej.sock") ["--port", "1234", "--unix", "nepackej.sock", "--port", "5678"]
        `shouldList` [PortNumber 1234, UnixSocket "nepackej.sock", PortNumber 5678]
        `through` _Right._Right.addrs.folded

  context "term" $ do
    it "submits to running pakej if no arguments are provided" $
      parse (parser "pakej.sock") []
        `shouldPreview` Submit `through` _Right._Right.prev

    it "replaces running pakej if --replace argument is provided" $
      parse (parser "pakej.sock") ["--replace"]
        `shouldPreview` Replace `through` _Right._Right.prev

    it "submits to running pakej if --submit argument is provided" $
      parse (parser "pakej.sock") ["--submit"]
        `shouldPreview` Submit `through` _Right._Right.prev

  context "hostname" $ do
    it "connects to localhost if no arguments are provided" $
      parse (parser "pakej.sock") []
        `shouldPreview` "localhost" `through` _Right._Right.host

    it "connects to another host if --hostname argument is provided" $
      parse (parser "pakej.sock") ["--hostname", "budueba.com"]
        `shouldPreview` "budueba.com" `through` _Right._Right.host

  context "mixed" $ do
    it "converts commands before options into queries" $
      parse (parser "pakej.sock") ["bar", "--port", "1234", "--hostname", "localhost"]
        `shouldPreview` CQuery "bar" `through` _Right._Right.mode._Client

    it "converts commands between options into queries" $
      parse (parser "pakej.sock") ["--port", "1234", "bar", "--hostname", "localhost"]
        `shouldPreview` CQuery "bar" `through` _Right._Right.mode._Client

    it "converts commands after options into queries" $
      parse (parser "pakej.sock") ["--port", "1234", "--hostname", "localhost", "bar"]
        `shouldPreview` CQuery "bar" `through` _Right._Right.mode._Client

parse :: ParserInfo a -> [String] -> Either ParserFailure a
parse = execParserPure (prefs mempty)
