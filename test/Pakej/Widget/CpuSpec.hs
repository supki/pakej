{-# LANGUAGE OverloadedStrings #-}
module Pakej.Widget.CpuSpec
  ( spec
  ) where

import Test.Hspec

import Pakej.Widget.Cpu


spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses total cpu usage info when given nothing" $
      parseLine Nothing "cpu 1 2 3 4" `shouldBe` Just (MkS [1, 2, 3, 4])

    it "parses single core usage info when given it as an argument" $
      parseLine (Just 0) "cpu0 5 6 7 8" `shouldBe` Just (MkS [5, 6, 7, 8])

    it "does not parse single core usage info when given nothing" $
      parseLine Nothing "cpu0 1 2 3 4" `shouldBe` noSnapshot

    it "does not parse total cpu usage info when given core number as an argument" $
      parseLine (Just 0) "cpu 1 2 3 4" `shouldBe` noSnapshot

  describe "parseSnapshot" $ do
    it "parses the first valid cpu usage record" $
      parseSnapshot (Just 1)
        "cpu0 1 2\n\
        \cpu1 2 3\n\
        \cpu1 3 4\n" `shouldBe` Just (MkS [2, 3])

    it "fails to parse if valid cpu usage record is available" $
      parseSnapshot (Just 17)
        "cpu0 1 2\n\
        \cpu1 2 3\n\
        \cpu1 3 4\n" `shouldBe` noSnapshot

  describe "getData" $ do
    it "does not throw exceptions if file does not exist" $
      getData "does-not-exist.txt"
     `shouldReturn`
      cpuDataError "does-not-exist.txt: openFile: does not exist (No such file or directory)"

    it "does not throw exceptions if file is of the wrong type" $
      getData "/"
     `shouldReturn`
      cpuDataError "/: openFile: inappropriate type (is a directory)"

  describe "computeUsage" $ do
    it "computes cpu usage between 2 snapshots" $ do
      s1 <- parseLine (Just 0) "cpu0 7928261 498437 3288444 106187410 2371849 52378 38421 0 0"
      s2 <- parseLine (Just 0) "cpu0 7928273 498437 3288455 106187766 2371849 52378 38421 0 0"
      return (computeUsage (snapshotDiff s1 s2))
     `shouldBe`
      Just 6.068601583113456

    it "computes zero cpu usage between 2 identical snapshots" $ do
      s1 <- parseLine (Just 0) "cpu0 7928261 498437 3288444 106187410 2371849 52378 38421 0 0"
      return (computeUsage (snapshotDiff s1 s1))
     `shouldBe`
      Just 0

noSnapshot :: Maybe (Snapshot Rational)
noSnapshot = Nothing
