{-# LANGUAGE OverloadedStrings #-}
module Pakej.Widget.MemorySpec
  ( spec
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
import           Numeric (showFFloat)
import           Test.Hspec

import           Pakej.Widget.Memory


spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses excerpt lines from the real /proc/meminfo" $
      parseLine "MemTotal:        3912112 kB" `shouldBe` Right ("MemTotal", 3912112)

    it "parses invalid lines to errors" $
      parseLine "foo bar kB" `shouldBe` Left "Pakej.Widget.Memory: bad line: foo bar kB"

  describe "parseData" $
    it "parses excerpt content from /proc/meminfo to the hash" $
      parseData memDataNew `shouldBe` Right (Mem (HashMap.fromList
        [ ("MemTotal", 2040896)
        , ("MemFree", 1253068)
        , ("MemAvailable", 1793956)
        , ("Buffers", 82356)
        , ("Cached", 447036)
        ]))

  describe "getData" $ do
    it "does not throw exceptions if file does not exist" $
      getData "does-not-exist.txt"
     `shouldReturn`
      memoryDataError "does-not-exist.txt: openFile: does not exist (No such file or directory)"

    it "does not throw exceptions if file is of the wrong type" $
      getData "/"
     `shouldReturn`
      memoryDataError "/: openFile: inappropriate type (is a directory)"

    it "does not throw exceptions if file is gibberish" $
      getData "/proc/cpuinfo"
     `shouldReturn`
      memoryDataError "bad line: processor\t: 0"

  context "queries" $ do
    describe "available" $ do
      it "uses MemAvailable if available" $
        fmap available (parseData memDataNew) `shouldBe` Right (Just 1793956)

      it "downgrades to approximate calculation if MemAvailable is not available" $
        fmap available (parseData memDataOld) `shouldBe` Right (Just 1782460)

    describe "ratio" $
      it "divides the results of two queries" $
        fmap (fmap showRatio . ratio used total) (parseData memDataNew) `shouldBe` Right (Just "0.12")

showRatio :: RealFloat a => a -> String
showRatio n = showFFloat (Just 2) n ""

memDataOld, memDataNew :: Text
memDataOld = Text.unlines
  [ "MemTotal:        2040896 kB"
  , "MemFree:         1253068 kB"
  , "Buffers:           82356 kB"
  , "Cached:           447036 kB"
  ]
memDataNew = Text.unlines
  [ "MemTotal:        2040896 kB"
  , "MemFree:         1253068 kB"
  , "MemAvailable:    1793956 kB"
  , "Buffers:           82356 kB"
  , "Cached:           447036 kB"
  ]
