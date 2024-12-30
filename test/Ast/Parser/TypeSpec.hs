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

  describe "Function Types" $ do
    it "parses int -> float" $ do
      parse P.parseType "" ": int -> float" `shouldBe` Right (AT.TFunction {AT.returnType = AT.TFloat, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False})

    it "parses int int -> void" $ do
      parse P.parseType "" ": int int -> void" `shouldBe` Right (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0, AT.TInt 0], AT.isVariadic = False})

  describe "Struct Types" $ do
    it "parses struct { name -> int }" $ do
      parse P.parseType "" "test :: struct { name -> int }" `shouldBe` Right (AT.TStruct {AT.structName = "test", AT.fields = [("name", AT.TInt 0)]})

    it "parses struct with multiple fields" $ do
      parse P.parseType "" "vec :: struct { x -> float y -> float }" `shouldBe` Right (AT.TStruct {AT.structName = "vec", AT.fields = [("x", AT.TFloat), ("y", AT.TFloat)]})

  describe "Union Types" $ do
    it "parses union { name -> int }" $ do
      parse P.parseType "" "test :: union { name -> int }" `shouldBe` Right (AT.TUnion {AT.unionName = "test", AT.variants = [("name", AT.TInt 0)]})

    it "parses union with multiple fields" $ do
      parse P.parseType "" "vec :: union { x -> float y -> float }" `shouldBe` Right (AT.TUnion {AT.unionName = "vec", AT.variants = [("x", AT.TFloat), ("y", AT.TFloat)]})

  describe "Typedefs" $ do
    it "parses typedef alias" $ do
      parse P.parseType "" "Alias :: int" `shouldBe` Right (AT.TTypedef "Alias" (AT.TInt 0))
