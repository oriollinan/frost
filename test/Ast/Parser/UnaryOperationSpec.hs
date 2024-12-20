module Ast.Parser.UnaryOperationSpec where

import qualified Ast.Parser.UnaryOperation as AUO
import qualified Ast.Types as AT
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

spec :: Spec
spec = do
  describe "parseUnaryOperation" $ do
    it "parses logical NOT" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "!x"
        `shouldBe` Right AT.Not

    it "parses 'not' logical unary operator" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "not x"
        `shouldBe` Right AT.Not

    it "parses bitwise NOT" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "~x"
        `shouldBe` Right AT.BitNot

    it "parses address-of operator" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "&x"
        `shouldBe` Right AT.AddrOf

    it "parses pre-unary increment" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "++x"
        `shouldBe` Right AT.PreInc

    it "parses pre-unary decrement" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "--x"
        `shouldBe` Right AT.PreDec

    it "parses dereference operator" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "x."
        `shouldBe` Right AT.Deref

    it "parses post-unary increment" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "x++"
        `shouldBe` Right AT.PostInc

    it "parses post-unary decrement" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "x--"
        `shouldBe` Right AT.PostDec

    it "returns error for invalid operator" $ do
      M.parse (AUO.parseUnaryOperation (MC.string "x")) "" "invalid"
        `shouldSatisfy` isLeft
