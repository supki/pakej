{-# LANGUAGE OverloadedStrings #-}
module Pakej.CommunicationSpec (spec) where

import Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Serialize (Serialize, encode, decode)
import Test.Hspec
import Test.Hspec.Expectations.Lens

import Pakej.Communication


spec :: Spec
spec = do
  describe "Client" $ do
    it "has working serialization for CQuery" $ do
      let query = CQuery "ДМИТРИЙ МАЛИКОВ"
      roundtrip query `shouldPreview` query `through` _Right

    it "has working serialization for CStatus" $
      roundtrip CStatus `shouldPreview` CStatus `through` _Right

    it "only uses tag 0" $
      decodeClient (ByteString.pack [0x02, 0x04, 0x07])
        `shouldPreview`
           [ "Failed reading: Unknown Pakej.Command.Client value tag: 2"
           , "Empty call stack"
           ]
        `through` _Left.to lines

  describe "Daemon" $ do
    it "has working serialization for DResponse" $ do
      let response = DQuery "ДМИТРИЙ МАЛИКОВ"
      roundtrip response `shouldPreview` response `through` _Right

    it "has working serialization for DStatus" $ do
      let status = DStatus ["foo", "bar", "baz"]
      roundtrip status `shouldPreview` status `through` _Right

    it "only uses tag 0" $
      decodeDaemon (ByteString.pack [0x02, 0x04, 0x07])
        `shouldPreview`
           [ "Failed reading: Unknown Pakej.Command.Daemon value tag: 2"
           , "Empty call stack"
           ]
        `through` _Left.to lines

roundtrip :: Serialize a => a -> Either String a
roundtrip = decode . encode

decodeClient :: ByteString -> Either String Client
decodeClient = decode

decodeDaemon :: ByteString -> Either String Daemon
decodeDaemon = decode
