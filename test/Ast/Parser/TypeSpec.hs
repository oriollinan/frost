module Ast.Parser.TypeSpec (spec) where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Type as P
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty)
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parseWithEnv input =
        fst $ S.runState (M.runParserT P.parseType "" input) E.emptyEnv
  let parseWithCustomEnv input env =
        fst $ S.runState (M.runParserT P.parseType "" input) env

  describe "Base Types" $ do
    it "parses int" $ do
      parseWithEnv "int" `shouldBe` Right (AT.TInt 0)

    it "parses float" $ do
      parseWithEnv "float" `shouldBe` Right AT.TFloat

    it "parses double" $ do
      parseWithEnv "double" `shouldBe` Right AT.TDouble

    it "parses char" $ do
      parseWithEnv "char" `shouldBe` Right AT.TChar

    it "parses bool" $ do
      parseWithEnv "bool" `shouldBe` Right AT.TBoolean

    it "parses void" $ do
      parseWithEnv "void" `shouldBe` Right AT.TVoid

  describe "Pointer Types" $ do
    it "parses *int" $ do
      parseWithEnv "*int" `shouldBe` Right (AT.TPointer (AT.TInt 0))

    it "parses *void" $ do
      parseWithEnv "*void" `shouldBe` Right (AT.TPointer AT.TVoid)

  describe "Mutable Types" $ do
    it "parses mutable int" $ do
      parseWithEnv "mutable int" `shouldBe` Right (AT.TMutable (AT.TInt 0))

  describe "Array Types" $ do
    it "parses []int" $ do
      parseWithEnv "[]int" `shouldBe` Right (AT.TArray (AT.TInt 0) Nothing)

    it "parses [10]int" $ do
      parseWithEnv "[10]int" `shouldBe` Right (AT.TArray (AT.TInt 0) (Just 10))

    it "parses []float" $ do
      parseWithEnv "[]float" `shouldBe` Right (AT.TArray AT.TFloat Nothing)

  describe "Function Types" $ do
    it "parses int -> float" $ do
      parseWithEnv ": int -> float" `shouldBe` Right (AT.TFunction {AT.returnType = AT.TFloat, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False})

    it "parses int int -> void" $ do
      parseWithEnv ": int int -> void" `shouldBe` Right (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0, AT.TInt 0], AT.isVariadic = False})

  describe "Struct Types" $ do
    it "parses struct { name -> int }" $ do
      parseWithEnv "test :: struct { name -> int }" `shouldBe` Right (AT.TStruct {AT.structName = "test", AT.fields = [("name", AT.TInt 0)]})

    it "parses struct with multiple fields" $ do
      parseWithEnv "vec :: struct { x -> float y -> float }" `shouldBe` Right (AT.TStruct {AT.structName = "vec", AT.fields = [("x", AT.TFloat), ("y", AT.TFloat)]})

  describe "Union Types" $ do
    it "parses union { name -> int }" $ do
      parseWithEnv "test :: union { name -> int }" `shouldBe` Right (AT.TUnion {AT.unionName = "test", AT.variants = [("name", AT.TInt 0)]})

    it "parses union with multiple fields" $ do
      parseWithEnv "vec :: union { x -> float y -> float }" `shouldBe` Right (AT.TUnion {AT.unionName = "vec", AT.variants = [("x", AT.TFloat), ("y", AT.TFloat)]})

  describe "Typedefs" $ do
    it "parses typedef alias" $ do
      parseWithEnv "Alias :: int" `shouldBe` Right (AT.TTypedef "Alias" (AT.TInt 0))

  describe "Custom Types" $ do
    it "parses a defined custom struct type" $ do
      let env = E.insertType "Point" (AT.TStruct "Point" [("x", AT.TInt 0), ("y", AT.TInt 0)]) E.emptyEnv
      parseWithCustomEnv "Point" env `shouldBe` Right (AT.TStruct "Point" [("x", AT.TInt 0), ("y", AT.TInt 0)])

    it "parses a defined custom alias type" $ do
      let env = E.insertType "Vector2" (AT.TTypedef "Vector2" (AT.TStruct "Point" [("x", AT.TInt 0), ("y", AT.TInt 0)])) E.emptyEnv
      parseWithCustomEnv "Vector2" env `shouldBe` Right (AT.TTypedef "Vector2" (AT.TStruct "Point" [("x", AT.TInt 0), ("y", AT.TInt 0)]))

    it "returns an error for an undefined custom type" $ do
      let env = E.emptyEnv
      parseWithCustomEnv "UnknownType" env `shouldSatisfy` isLeft

    it "parses nested custom types" $ do
      let pointType = AT.TStruct "Point" [("x", AT.TInt 0), ("y", AT.TInt 0)]
      let env =
            E.insertType "Point" pointType $
              E.insertType "Shape" (AT.TStruct "Shape" [("center", pointType)]) E.emptyEnv
      parseWithCustomEnv "Shape" env `shouldBe` Right (AT.TStruct "Shape" [("center", pointType)])
