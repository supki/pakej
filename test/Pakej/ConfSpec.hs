{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pakej.ConfSpec (spec) where

import Control.Lens
import Network (PortID(..))
import Options.Applicative
import Test.Hspec
import Test.Hspec.Expectations.Lens

import Pakej.Protocol
import Pakej.Conf

import Paths_pakej (version)


spec :: Spec
spec = do
  context "version" $ do
    it "shows verion if --version is provided" $
      execParserMaybe (parser defaultPakejConf) ["--version"]
        `shouldHave` _Just._Left.only version

    it "shows verion if -v is provided" $
      execParserMaybe (parser defaultPakejConf) ["-v"]
        `shouldHave` _Just._Left.only version

  context "mode" $ do
    it "starts as daemon if no command arguments are provided" $
      execParserMaybe (parser defaultPakejConf) []
        `shouldHave` _Just._Right.mode._Daemon

    it "converts commands into queries" $
      execParserMaybe (parser defaultPakejConf) ["bar"]
        `shouldHave` _Just._Right.mode._Query.only (CQuery "bar")

    it "converts --stat option into a status query" $
      execParserMaybe (parser defaultPakejConf) ["--stat"]
        `shouldHave` _Just._Right.mode._Query.only CStatus

    it "converts --repl option into a repl session" $
      execParserMaybe (parser defaultPakejConf) ["--repl"]
        `shouldHave` _Just._Right.mode._Repl

  context "addr" $ do
    it "uses unix socket if no command arguments are provided" $
      execParserMaybe (parser defaultPakejConf) []
        `shouldHave` _Just._Right.addrs.only [UnixSocket "pakej.sock"]

    it "uses unix socket if --unix argument is provided" $
      execParserMaybe (parser defaultPakejConf) ["--unix", "nepakej.sock"]
        `shouldHave` _Just._Right.addrs.only [UnixSocket "nepakej.sock"]

    it "uses port number if --port argument is provided" $
      execParserMaybe (parser defaultPakejConf) ["--port", "1234"]
        `shouldHave` _Just._Right.addrs.only [PortNumber 1234]

    it "supports multiple socket arguments" $
      execParserMaybe (parser defaultPakejConf) ["--port", "1234", "--unix", "nepackej.sock", "--port", "5678"]
     `shouldHave`
      _Just._Right.addrs.only [PortNumber 1234, UnixSocket "nepackej.sock", PortNumber 5678]

  context "term" $ do
    it "submits to running pakej if no arguments are provided" $
      execParserMaybe (parser defaultPakejConf) []
        `shouldHave` _Just._Right.policy.only Respect

    it "replaces running pakej if --replace argument is provided" $
      execParserMaybe (parser defaultPakejConf) ["--replace"]
        `shouldHave` _Just._Right.policy.only Replace

    it "submits to running pakej if --respect argument is provided" $
      execParserMaybe (parser defaultPakejConf) ["--respect"]
        `shouldHave` _Just._Right.policy.only Respect

  context "hostname" $ do
    it "connects to localhost if no arguments are provided" $
      execParserMaybe (parser defaultPakejConf) []
        `shouldHave` _Just._Right.host.only "localhost"

    it "connects to another host if --hostname argument is provided" $
      execParserMaybe (parser defaultPakejConf) ["--hostname", "budueba.com"]
        `shouldHave` _Just._Right.host.only "budueba.com"

  context "daemonization" $ do
    it "daemonizes itself by default" $
      execParserMaybe (parser defaultPakejConf) []
        `shouldHave` _Just._Right.foreground.only False

    it "stays in the foreground if --foreground is provided" $
      execParserMaybe (parser defaultPakejConf) ["--foreground"]
        `shouldHave` _Just._Right.foreground.only True

    it "stays in the foreground if -f is provided" $
      execParserMaybe (parser defaultPakejConf) ["-f"]
        `shouldHave` _Just._Right.foreground.only True

  context "mixed" $ do
    it "converts commands before options into queries" $
      execParserMaybe (parser defaultPakejConf) ["bar", "--port", "1234", "--hostname", "localhost"]
        `shouldHave` _Just._Right.mode._Query.only (CQuery "bar")

    it "converts commands between options into queries" $
      execParserMaybe (parser defaultPakejConf) ["--port", "1234", "bar", "--hostname", "localhost"]
        `shouldHave` _Just._Right.mode._Query.only (CQuery "bar")

    it "converts commands after options into queries" $
      execParserMaybe (parser defaultPakejConf) ["--port", "1234", "--hostname", "localhost", "bar"]
        `shouldHave` _Just._Right.mode._Query.only (CQuery "bar")
