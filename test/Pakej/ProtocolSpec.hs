{-# LANGUAGE OverloadedStrings #-}
module Pakej.ProtocolSpec (spec) where

import           Control.Lens
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.Serialize (Serialize, encode, decode)
import           Test.Hspec
import           Test.Hspec.Expectations.Lens

import           Pakej.Protocol


spec :: Spec
spec = do
  describe "Request" $ do
    it "has working serialization for CQuery" $ do
      let query = CQuery "ДМИТРИЙ МАЛИКОВ"
      roundtrip query `shouldPreview` query `through` _Right

    it "has working serialization for CStatus" $
      roundtrip CStatus `shouldPreview` CStatus `through` _Right

    it "only uses tags < 2" $
      decodeRequest (ByteString.pack [0x02, 0x04, 0x07])
        `shouldPreview`
           [ "Failed reading: Unknown Pakej.Command.Client value tag: 2"
           , "Empty call stack"
           ]
        `through` _Left.to lines

  describe "Response" $ do
    it "has working serialization for DResponse with payload" $ do
      let response = DQuery (Just "ДМИТРИЙ МАЛИКОВ")
      roundtrip response `shouldPreview` response `through` _Right

    it "has working serialization for DResponse without payload" $ do
      let response = DQuery Nothing
      roundtrip response `shouldPreview` response `through` _Right

    it "has working serialization for DStatus" $ do
      let status = DStatus ["foo", "bar", "baz"]
      roundtrip status `shouldPreview` status `through` _Right

    it "only uses tags < 2" $
      decodeResponse (ByteString.pack [0x02, 0x04, 0x07])
        `shouldPreview`
           [ "Failed reading: Unknown Pakej.Command.Daemon value tag: 2"
           , "Empty call stack"
           ]
        `through` _Left.to lines

roundtrip :: Serialize a => a -> Either String a
roundtrip = decode . encode

decodeRequest :: ByteString -> Either String Request
decodeRequest = decode

decodeResponse :: ByteString -> Either String Response
decodeResponse = decode
