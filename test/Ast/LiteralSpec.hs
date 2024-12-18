module Ast.LiteralSpec (spec) where

import qualified Ast.Literal as AL
import qualified Ast.Types as AT
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  describe "parseInt" $ do
    it "parses positive integers" $ do
      M.parse AL.parseLiteral "" "123" `shouldBe` Right (AT.LInt 123)

    it "parses negative integers" $ do
      M.parse AL.parseLiteral "" "-456" `shouldBe` Right (AT.LInt (-456))

    it "fails on non-integer input" $ do
      isLeft (M.parse AL.parseLiteral "" "abc") `shouldBe` True

  describe "parseFloat" $ do
    it "parses positive floats" $ do
      M.parse AL.parseLiteral "" "123.45" `shouldBe` Right (AT.LFloat 123.45)

    it "parses negative floats" $ do
      M.parse AL.parseLiteral "" "-67.89" `shouldBe` Right (AT.LFloat (-67.89))

    it "fails on non-float input" $ do
      isLeft (M.parse AL.parseLiteral "" "abc") `shouldBe` True

  describe "parseBool" $ do
    it "parses true" $ do
      M.parse AL.parseLiteral "" "true" `shouldBe` Right (AT.LBool True)

    it "parses false" $ do
      M.parse AL.parseLiteral "" "false" `shouldBe` Right (AT.LBool False)

    it "fails on invalid input" $ do
      isLeft (M.parse AL.parseLiteral "" "maybe") `shouldBe` True

  describe "parseChar" $ do
    it "parses a single character" $ do
      M.parse AL.parseLiteral "" "'a'" `shouldBe` Right (AT.LChar 'a')

    it "fails on invalid input" $ do
      isLeft (M.parse AL.parseLiteral "" "'abc'") `shouldBe` True

  describe "parseArray" $ do
    it "parses an array of integers" $ do
      M.parse AL.parseLiteral "" "[1,2,3]" `shouldBe` Right (AT.LArray [AT.LInt 1, AT.LInt 2, AT.LInt 3])

    it "parses an array of mixed literals" $ do
      M.parse AL.parseLiteral "" "[true,'a',123]" `shouldBe` Right (AT.LArray [AT.LBool True, AT.LChar 'a', AT.LInt 123])

    it "parses a string literal as an array of characters" $ do
      M.parse AL.parseLiteral "" "\"hello\"" `shouldBe` Right (AT.LArray (map AT.LChar "hello"))

    it "fails on invalid input" $ do
      isLeft (M.parse AL.parseLiteral "" "[1,true,]") `shouldBe` True

  describe "parseNull" $ do
    it "parses null" $ do
      M.parse AL.parseLiteral "" "null" `shouldBe` Right AT.LNull

    it "fails on non-null input" $ do
      isLeft (M.parse AL.parseLiteral "" "none") `shouldBe` True
