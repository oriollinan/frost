module Ast.Parser.LiteralSpec (spec) where

import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.State as PS
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Data.Either as E
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parse input = do
        (result, _) <- S.runStateT (M.runParserT PL.parseLiteral "" input) PS.parserState
        return result
  let parseCustom env input = do
        (result, _) <- S.runStateT (M.runParserT PL.parseLiteral "" input) env
        return result

  describe "parseInt" $ do
    it "parses positive integers" $ do
      let input = "123"
      result <- parse input
      let expected = Right (AT.LInt 123)
      result `shouldBe` expected

    it "parses negative integers" $ do
      let input = "-456"
      result <- parse input
      let expected = Right (AT.LInt (-456))
      result `shouldBe` expected

    it "fails on non-integer input" $ do
      let input = "abc"
      result <- parse input
      E.isLeft result `shouldBe` True

  describe "parseFloat" $ do
    it "parses positive floats" $ do
      let input = "123,45"
      result <- parse input
      let expected = Right (AT.LFloat 123.45)
      result `shouldBe` expected

    it "parses negative floats" $ do
      let input = "-67,89"
      result <- parse input
      let expected = Right (AT.LFloat (-67.89))
      result `shouldBe` expected

    it "fails on non-float input" $ do
      let input = "abc"
      result <- parse input
      E.isLeft result `shouldBe` True

  describe "parseBool" $ do
    it "parses true" $ do
      let input = "true"
      result <- parse input
      let expected = Right (AT.LBool True)
      result `shouldBe` expected

    it "parses false" $ do
      let input = "false"
      result <- parse input
      let expected = Right (AT.LBool False)
      result `shouldBe` expected

    it "fails on invalid input" $ do
      let input = "maybe"
      result <- parse input
      E.isLeft result `shouldBe` True

  describe "parseChar" $ do
    it "parses a single character" $ do
      let input = "'a'"
      result <- parse input
      let expected = Right (AT.LChar 'a')
      result `shouldBe` expected

    it "fails on invalid input" $ do
      let input = "'abc'"
      result <- parse input
      E.isLeft result `shouldBe` True

  describe "parseArray" $ do
    it "parses an array of integers" $ do
      let input = "[1 2 3]"
      result <- parse input
      let expected = Right (AT.LArray [AT.LInt 1, AT.LInt 2, AT.LInt 3])
      result `shouldBe` expected

    it "parses an array of mixed literals" $ do
      let input = "[true 'a' 123]"
      result <- parse input
      let expected = Right (AT.LArray [AT.LBool True, AT.LChar 'a', AT.LInt 123])
      result `shouldBe` expected

    it "parses a string literal as an array of characters" $ do
      let input = "\"hello\""
      result <- parse input
      let expected = Right (AT.LArray (map AT.LChar "hello"))
      result `shouldBe` expected

    it "fails on invalid input" $ do
      let input = "[1 true ]"
      result <- parse input
      E.isLeft result `shouldBe` True

  describe "parseNull" $ do
    it "parses null" $ do
      let input = "null"
      result <- parse input
      let expected = Right AT.LNull
      result `shouldBe` expected

    it "fails on non-null input" $ do
      let input = "none"
      result <- parse input
      E.isLeft result `shouldBe` True

  describe "Parse a Struct Literal" $ do
    it "parses a struct with 1 field" $ do
      let input = "Packet { data = \"\" }"
      let structType = AT.TStruct "Packet" [("data", AT.TArray AT.TChar Nothing)]
      let env = PS.insertType "Packet" structType PS.parserState
      result <- parseCustom env input
      let expected = Right $ AT.LStruct [("data", AT.LArray [])]
      result `shouldBe` expected
