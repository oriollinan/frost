module Ast.Parser.TypeSpec (spec) where

import qualified Ast.Parser.Type as P
import qualified Ast.Types as AT
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, parse)

spec :: Spec
spec = do
  describe "Base Types" $ do
    it "parses int" $ do
      parse P.parseType "" "int" `shouldBe` Right (AT.TInt 0)

    it "parses float" $ do
      parse P.parseType "" "float" `shouldBe` Right AT.TFloat

    it "parses double" $ do
      parse P.parseType "" "double" `shouldBe` Right AT.TDouble

    it "parses char" $ do
      parse P.parseType "" "char" `shouldBe` Right AT.TChar

    it "parses bool" $ do
      parse P.parseType "" "bool" `shouldBe` Right AT.TBoolean

    it "parses void" $ do
      parse P.parseType "" "void" `shouldBe` Right AT.TVoid

  describe "Pointer Types" $ do
    it "parses *int" $ do
      parse P.parseType "" "*int" `shouldBe` Right (AT.TPointer (AT.TInt 0))

    it "parses *void" $ do
      parse P.parseType "" "*void" `shouldBe` Right (AT.TPointer AT.TVoid)

  describe "Mutable Types" $ do
    it "parses mutable int" $ do
      parse P.parseType "" "mutable int" `shouldBe` Right (AT.TMutable (AT.TInt 0))

  describe "Array Types" $ do
    it "parses int[10]" $ do
      parse P.parseType "" "int[10]" `shouldBe` Right (AT.TArray (AT.TInt 0) (Just 10))

    it "parses int[]" $ do
      parse P.parseType "" "int[]" `shouldBe` Right (AT.TArray (AT.TInt 0) Nothing)

  describe "Function Types" $ do
    it "parses int -> float" $ do
      parse P.parseType "" "int -> float" `shouldBe` Right (AT.TFunction AT.TFloat [AT.TInt 0] False)

    it "parses int int -> void" $ do
      parse P.parseType "" "int int -> void" `shouldBe` Right (AT.TFunction AT.TVoid [AT.TInt 0, AT.TInt 0] False)

  describe "Struct Types" $ do
    it "parses struct { name -> int }" $ do
      parse P.parseType "" "test :: struct { name -> int }" `shouldBe` Right (AT.TStruct "test" [("name", AT.TInt 0)])

    it "parses struct with multiple fields" $ do
      parse P.parseType "" "vec :: struct { x -> float, y -> float }" `shouldBe` Right (AT.TStruct "vec" [("x", AT.TFloat), ("y", AT.TFloat)])

  describe "Union Types" $ do
    it "parses union { success -> int, error -> *char }" $ do
      parse P.parseType "" "response :: union { success -> int, error -> *char }" `shouldBe` Right (AT.TUnion "response" [("success", AT.TInt 0), ("error", AT.TPointer AT.TChar)])
