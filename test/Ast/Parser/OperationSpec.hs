module Ast.Parser.OperationSpec where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Operation as AO
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let initialEnv = E.emptyEnv
  let parseWithEnv input = fst $ S.runState (M.runParserT AO.parseOperation "" input) initialEnv

  describe "parseOperation" $ do
    it "parses addition operator" $ do
      parseWithEnv "+" `shouldBe` Right AT.Add

    it "parses subtraction operator" $ do
      parseWithEnv "-" `shouldBe` Right AT.Sub

    it "parses multiplication operator" $ do
      parseWithEnv "*" `shouldBe` Right AT.Mul

    it "parses division operator" $ do
      parseWithEnv "/" `shouldBe` Right AT.Div

    it "parses modulo operator" $ do
      parseWithEnv "mod" `shouldBe` Right AT.Mod

    it "parses bitwise AND operator" $ do
      parseWithEnv "&" `shouldBe` Right AT.BitAnd

    it "parses bitwise OR operator" $ do
      parseWithEnv "|" `shouldBe` Right AT.BitOr

    it "parses bitwise XOR operator" $ do
      parseWithEnv "^" `shouldBe` Right AT.BitXor

    it "parses left shift operator" $ do
      parseWithEnv "<<" `shouldBe` Right AT.BitShl

    it "parses right shift operator" $ do
      parseWithEnv ">>" `shouldBe` Right AT.BitShr

    it "parses less than operator" $ do
      parseWithEnv "<" `shouldBe` Right AT.Lt

    it "parses greater than operator" $ do
      parseWithEnv ">" `shouldBe` Right AT.Gt

    it "parses less than or equal to operator" $ do
      parseWithEnv "<=" `shouldBe` Right AT.Lte

    it "parses greater than or equal to operator" $ do
      parseWithEnv ">=" `shouldBe` Right AT.Gte

    it "parses equality operator" $ do
      parseWithEnv "==" `shouldBe` Right AT.Eq

    it "parses 'is' equality operator" $ do
      parseWithEnv "is" `shouldBe` Right AT.Eq

    it "parses inequality operator" $ do
      parseWithEnv "!=" `shouldBe` Right AT.Ne

    it "parses logical AND operator" $ do
      parseWithEnv "&&" `shouldBe` Right AT.And

    it "parses 'and' logical operator" $ do
      parseWithEnv "and" `shouldBe` Right AT.And

    it "parses logical OR operator" $ do
      parseWithEnv "||" `shouldBe` Right AT.Or

    it "parses 'or' logical operator" $ do
      parseWithEnv "or" `shouldBe` Right AT.Or

    it "returns error for invalid operator" $ do
      let result = parseWithEnv "invalid"
      isLeft result `shouldBe` True
