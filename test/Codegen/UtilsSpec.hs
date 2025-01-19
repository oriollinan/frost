{-# LANGUAGE OverloadedStrings #-}

module Codegen.UtilsSpec (spec) where

import qualified Codegen.Utils as CU
import qualified Data.ByteString.Short as BS
import qualified LLVM.AST as AST
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "Codegen.Utils" $ do
  H.describe "stringToByteString" $ do
    H.it "should convert empty string" $ do
      CU.stringToByteString "" `H.shouldBe` BS.empty

    H.it "should convert ASCII string" $ do
      CU.stringToByteString "hello" `H.shouldBe` BS.pack [104, 101, 108, 108, 111]

    H.it "should convert string with special characters" $ do
      CU.stringToByteString "hello\n\t" `H.shouldBe` BS.pack [104, 101, 108, 108, 111, 10, 9]

  H.describe "byteStringToString" $ do
    H.it "should convert empty bytestring" $ do
      CU.byteStringToString BS.empty `H.shouldBe` ""

    H.it "should convert ASCII bytestring" $ do
      CU.byteStringToString (BS.pack [104, 101, 108, 108, 111]) `H.shouldBe` "hello"

    H.it "should convert bytestring with special characters" $ do
      CU.byteStringToString (BS.pack [104, 101, 108, 108, 111, 10, 9]) `H.shouldBe` "hello\n\t"

  H.describe "nameToString" $ do
    H.it "should convert named identifier" $ do
      CU.nameToString (AST.Name "test") `H.shouldBe` "test"

    H.it "should convert unnamed identifier" $ do
      CU.nameToString (AST.UnName 42) `H.shouldBe` "42"

    H.it "should convert empty named identifier" $ do
      CU.nameToString (AST.Name "") `H.shouldBe` ""

    H.it "should handle special characters in named identifier" $ do
      CU.nameToString (AST.Name "_test.123") `H.shouldBe` "_test.123"
