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
  let initialEnv = PS.parserState
  let parseWithEnv input = fst $ S.runState (M.runParserT PL.parseLiteral "" input) initialEnv

  describe "parseInt" $ do
    it "parses positive integers" $ do
      parseWithEnv "123" `shouldBe` Right (AT.LInt 123)

    it "parses negative integers" $ do
      parseWithEnv "-456" `shouldBe` Right (AT.LInt (-456))

    it "fails on non-integer input" $ do
      E.isLeft (parseWithEnv "abc") `shouldBe` True

  describe "parseFloat" $ do
    it "parses positive floats" $ do
      parseWithEnv "123.45" `shouldBe` Right (AT.LFloat 123.45)

    it "parses negative floats" $ do
      parseWithEnv "-67.89" `shouldBe` Right (AT.LFloat (-67.89))

    it "fails on non-float input" $ do
      E.isLeft (parseWithEnv "abc") `shouldBe` True

  describe "parseBool" $ do
    it "parses true" $ do
      parseWithEnv "true" `shouldBe` Right (AT.LBool True)

    it "parses false" $ do
      parseWithEnv "false" `shouldBe` Right (AT.LBool False)

    it "fails on invalid input" $ do
      E.isLeft (parseWithEnv "maybe") `shouldBe` True

  describe "parseChar" $ do
    it "parses a single character" $ do
      parseWithEnv "'a'" `shouldBe` Right (AT.LChar 'a')

    it "fails on invalid input" $ do
      E.isLeft (parseWithEnv "'abc'") `shouldBe` True

  describe "parseArray" $ do
    it "parses an array of integers" $ do
      parseWithEnv "[1,2,3]" `shouldBe` Right (AT.LArray [AT.LInt 1, AT.LInt 2, AT.LInt 3])

    it "parses an array of mixed literals" $ do
      parseWithEnv "[true,'a',123]" `shouldBe` Right (AT.LArray [AT.LBool True, AT.LChar 'a', AT.LInt 123])

    it "parses a string literal as an array of characters" $ do
      parseWithEnv "\"hello\"" `shouldBe` Right (AT.LArray (map AT.LChar "hello"))

    it "fails on invalid input" $ do
      E.isLeft (parseWithEnv "[1,true,]") `shouldBe` True

  describe "parseNull" $ do
    it "parses null" $ do
      parseWithEnv "null" `shouldBe` Right AT.LNull

    it "fails on non-null input" $ do
      E.isLeft (parseWithEnv "none") `shouldBe` True
