module Ast.Parser.TypeSpec (spec) where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Type as PT
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parse input = do
        (result, _) <- S.runStateT (M.runParserT PT.parseType "" input) PS.parserState
        return result
  let parseCustom env input = do
        (result, _) <- S.runStateT (M.runParserT PT.parseType "" input) env
        return result

  describe "Base Types" $ do
    it "parses int" $ do
      let input = "int"
      result <- parse input
      let expected = Right (AT.TInt 32)
      result `shouldBe` expected

    it "parses custon int" $ do
      let input = "int8"
      result <- parse input
      let expected = Right (AT.TInt 8)
      result `shouldBe` expected

    it "parses float" $ do
      let input = "float"
      result <- parse input
      let expected = Right AT.TFloat
      result `shouldBe` expected

    it "parses double" $ do
      let input = "double"
      result <- parse input
      let expected = Right AT.TDouble
      result `shouldBe` expected

    it "parses char" $ do
      let input = "char"
      result <- parse input
      let expected = Right AT.TChar
      result `shouldBe` expected

    it "parses bool" $ do
      let input = "bool"
      result <- parse input
      let expected = Right AT.TBoolean
      result `shouldBe` expected

    it "parses never" $ do
      let input = "never"
      result <- parse input
      let expected = Right AT.TVoid
      result `shouldBe` expected

    it "parses custom int" $ do
      let input = "int64"
      result <- parse input
      let expected = Right (AT.TInt 64)
      result `shouldBe` expected

  describe "Pointer Types" $ do
    it "parses *int" $ do
      let input = "*int"
      result <- parse input
      let expected = Right (AT.TPointer (AT.TInt 32))
      result `shouldBe` expected

  describe "Mutable Types" $ do
    it "parses mutable int" $ do
      let input = "mut int"
      result <- parse input
      let expected = Right (AT.TMutable (AT.TInt 32))
      result `shouldBe` expected

  describe "Array Types" $ do
    it "parses []int" $ do
      let input = "[]int"
      result <- parse input
      let expected = Right (AT.TArray (AT.TInt 32) Nothing)
      result `shouldBe` expected

    it "parses [10]int" $ do
      let input = "[10]int"
      result <- parse input
      let expected = Right (AT.TArray (AT.TInt 32) (Just 10))
      result `shouldBe` expected

    it "parses []float" $ do
      let input = "[]float"
      result <- parse input
      let expected = Right (AT.TArray AT.TFloat Nothing)
      result `shouldBe` expected

  describe "Function Types" $ do
    it "parses int -> float" $ do
      let input = "int -> float"
      result <- parse input
      let expected = Right (AT.TFunction {AT.returnType = AT.TFloat, AT.paramTypes = [AT.TInt 32], AT.isVariadic = False})
      result `shouldBe` expected

    it "parses int int -> never" $ do
      let input = "int int -> never"
      result <- parse input
      let expected = Right (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 32, AT.TInt 32], AT.isVariadic = False})
      result `shouldBe` expected

    it "parses a HOF that takes a function" $ do
      let input = "(int -> int) -> never"
      result <- parse input
      let expected = Right $ AT.TFunction AT.TVoid [AT.TFunction (AT.TInt 32) [AT.TInt 32] False] False
      result `shouldBe` expected

    it "parses a HOF that returns a function" $ do
      let input = "never -> (int -> int)"
      result <- parse input
      let expected = Right $ AT.TFunction (AT.TFunction (AT.TInt 32) [AT.TInt 32] False) [AT.TVoid] False
      result `shouldBe` expected

    it "parses a variadic function" $ do
      let input = "*byte ... -> int"
      result <- parse input
      let expected = Right $ AT.TFunction (AT.TInt 32) [AT.TPointer $ AT.TInt 8] True
      result `shouldBe` expected

  describe "Custom Types" $ do
    it "parses a defined custom struct type" $ do
      let input = "Point"
      let env = PS.insertType "Point" (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]) PS.parserState
      result <- parseCustom env input
      let expected = Right (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)])
      result `shouldBe` expected

    it "parses a defined custom alias type" $ do
      let input = "Vector2"
      let env = PS.insertType "Vector2" (AT.TTypedef "Vector2" (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)])) PS.parserState
      result <- parseCustom env input
      let expected = Right (AT.TTypedef "Vector2" (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]))
      result `shouldBe` expected

    it "returns an error for an undefined custom type" $ do
      let input = "UnknownType"
      result <- parse input
      result `shouldSatisfy` isLeft

    it "parses nested custom types" $ do
      let input = "Shape"
      let pointType = AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]
      let env =
            PS.insertType "Point" pointType $
              PS.insertType "Shape" (AT.TStruct "Shape" [("center", pointType)]) PS.parserState
      result <- parseCustom env input
      let expected = Right (AT.TStruct "Shape" [("center", pointType)])
      result `shouldBe` expected
