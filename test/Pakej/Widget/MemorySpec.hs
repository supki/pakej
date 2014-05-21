{-# LANGUAGE OverloadedStrings #-}
module Pakej.Widget.MemorySpec
  ( spec
  ) where

import qualified Data.Text as Text
import qualified Data.HashMap.Strict as HashMap
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
      parseData (Text.unlines
        [ "MemTotal:        3912112 kB"
        , "MemFree:          168068 kB"
        , "Buffers:           54864 kB"
        , "Cached:          1353200 kB"
        ]) `shouldBe` Right (HashMap.fromList
          [ ("MemTotal", 3912112)
          , ("MemFree", 168068)
          , ("Buffers", 54864)
          , ("Cached", 1353200)
          ])

  describe "memoryData" $ do
    it "does not throw exceptions if file does not exist" $
      memoryData "does-not-exist.txt"
     `shouldReturn`
      memoryDataError "does-not-exist.txt: openFile: does not exist (No such file or directory)"

    it "does not throw exceptions if file is of the wrong type" $
      memoryData "/"
     `shouldReturn`
      memoryDataError "/: openFile: inappropriate type (is a directory)"

    it "does not throw exceptions if file is gibberish" $
      memoryData "/proc/cpuinfo"
     `shouldReturn`
      memoryDataError "bad line: processor\t: 0"
