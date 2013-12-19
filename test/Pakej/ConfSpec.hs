{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pakej.ConfSpec (spec) where

import Control.Lens
import Data.Monoid (mempty)
import Options.Applicative
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Expectations.Lens

import Pakej.Conf


instance Show ParserFailure where
  show _ = "<ParserFailure>"

spec :: Spec
spec = do
  prop "starts as daemon if no command arguments are provided" $ \xs ->
    parse (parser xs) [] `shouldHave` _Right._Daemon

  it "converts provided options into commands" $
    parse (parser ["foo", "bar", "baz"]) ["bar"] `shouldPreview` "bar" `through` _Right._Client

  it "does not have commands outside of options list" $
    parse (parser ["foo", "bar", "baz"]) ["xyzzy"] `shouldHave` _Left

parse :: ParserInfo a -> [String] -> Either ParserFailure a
parse = execParserPure (prefs mempty)
