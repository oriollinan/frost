module Ast.OperationSpec where

import qualified Ast.Operation as AO
import qualified Ast.Types as AT
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  describe "parseOperation" $ do
    it "parses addition operator" $ do
      M.parse AO.parseOperation "" "+" `shouldBe` Right AT.Add

    it "parses subtraction operator" $ do
      M.parse AO.parseOperation "" "-" `shouldBe` Right AT.Sub

    it "parses multiplication operator" $ do
      M.parse AO.parseOperation "" "*" `shouldBe` Right AT.Mul

    it "parses division operator" $ do
      M.parse AO.parseOperation "" "/" `shouldBe` Right AT.Div

    it "parses modulo operator" $ do
      M.parse AO.parseOperation "" "mod" `shouldBe` Right AT.Mod

    it "parses bitwise AND operator" $ do
      M.parse AO.parseOperation "" "&" `shouldBe` Right AT.BitAnd

    it "parses bitwise OR operator" $ do
      M.parse AO.parseOperation "" "|" `shouldBe` Right AT.BitOr

    it "parses bitwise XOR operator" $ do
      M.parse AO.parseOperation "" "^" `shouldBe` Right AT.BitXor

    it "parses left shift operator" $ do
      M.parse AO.parseOperation "" "<<" `shouldBe` Right AT.BitShl

    it "parses right shift operator" $ do
      M.parse AO.parseOperation "" ">>" `shouldBe` Right AT.BitShr

    it "parses less than operator" $ do
      M.parse AO.parseOperation "" "<" `shouldBe` Right AT.Lt

    it "parses greater than operator" $ do
      M.parse AO.parseOperation "" ">" `shouldBe` Right AT.Gt

    it "parses less than or equal to operator" $ do
      M.parse AO.parseOperation "" "<=" `shouldBe` Right AT.Lte

    it "parses greater than or equal to operator" $ do
      M.parse AO.parseOperation "" ">=" `shouldBe` Right AT.Gte

    it "parses equality operator" $ do
      M.parse AO.parseOperation "" "==" `shouldBe` Right AT.Eq

    it "parses 'is' equality operator" $ do
      M.parse AO.parseOperation "" "is" `shouldBe` Right AT.Eq

    it "parses inequality operator" $ do
      M.parse AO.parseOperation "" "!=" `shouldBe` Right AT.Ne

    it "parses logical AND operator" $ do
      M.parse AO.parseOperation "" "&&" `shouldBe` Right AT.And

    it "parses 'and' logical operator" $ do
      M.parse AO.parseOperation "" "and" `shouldBe` Right AT.And

    it "parses logical OR operator" $ do
      M.parse AO.parseOperation "" "||" `shouldBe` Right AT.And

    it "parses 'or' logical operator" $ do
      M.parse AO.parseOperation "" "or" `shouldBe` Right AT.And

    it "returns error for invalid operator" $ do
      M.parse AO.parseOperation "" "invalid" `shouldSatisfy` isLeft
