module Ast.Parser.UnaryOperationSpec where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.UnaryOperation as AUO
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let initialEnv = E.emptyEnv
  let parseWithEnv input opType =
        fst $ S.runState (M.runParserT (AUO.parseUnaryOperation opType) "" input) initialEnv

  describe "parseUnaryOperation" $ do
    it "parses logical NOT" $ do
      parseWithEnv "!" AUO.Pre `shouldBe` Right AT.Not

    it "parses 'not' logical unary operator" $ do
      parseWithEnv "not" AUO.Pre `shouldBe` Right AT.Not

    it "parses bitwise NOT" $ do
      parseWithEnv "~" AUO.Pre `shouldBe` Right AT.BitNot

    it "parses address-of operator" $ do
      parseWithEnv "&" AUO.Pre `shouldBe` Right AT.AddrOf

    it "parses pre-unary increment" $ do
      parseWithEnv "++" AUO.Pre `shouldBe` Right AT.PreInc

    it "parses pre-unary decrement" $ do
      parseWithEnv "--" AUO.Pre `shouldBe` Right AT.PreDec

    it "parses dereference operator" $ do
      parseWithEnv "." AUO.Post `shouldBe` Right AT.Deref

    it "parses post-unary increment" $ do
      parseWithEnv "++" AUO.Post `shouldBe` Right AT.PostInc

    it "parses post-unary decrement" $ do
      parseWithEnv "--" AUO.Post `shouldBe` Right AT.PostDec

    it "returns error for invalid operator" $ do
      let result = parseWithEnv "invalid" AUO.Pre
      isLeft result `shouldBe` True
