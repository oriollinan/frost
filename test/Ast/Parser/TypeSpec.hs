module Ast.Parser.TypeSpec (spec) where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Type as P
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parseWithEnv input =
        fst $ S.runState (M.runParserT P.parseType "" input) E.emptyEnv
  let parseWithCustomEnv input env =
        fst $ S.runState (M.runParserT P.parseType "" input) env

  describe "Base Types" $ do
    it "parses int" $ do
      parseWithEnv "int" `shouldBe` Right (AT.TInt 32)

    it "parses custon int" $ do
      parseWithEnv "int8" `shouldBe` Right (AT.TInt 8)

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
      parseWithEnv "*int" `shouldBe` Right (AT.TPointer (AT.TInt 32))

    it "parses *void" $ do
      parseWithEnv "*void" `shouldBe` Right (AT.TPointer AT.TVoid)

  describe "Mutable Types" $ do
    it "parses mutable int" $ do
      parseWithEnv "mut int" `shouldBe` Right (AT.TMutable (AT.TInt 32))

  describe "Array Types" $ do
    it "parses []int" $ do
      parseWithEnv "[]int" `shouldBe` Right (AT.TArray (AT.TInt 32) Nothing)

    it "parses [10]int" $ do
      parseWithEnv "[10]int" `shouldBe` Right (AT.TArray (AT.TInt 32) (Just 10))

    it "parses []float" $ do
      parseWithEnv "[]float" `shouldBe` Right (AT.TArray AT.TFloat Nothing)

  describe "Function Types" $ do
    it "parses int -> float" $ do
      parseWithEnv "(int) -> (float)" `shouldBe` Right (AT.TFunction {AT.returnType = AT.TFloat, AT.paramTypes = [AT.TInt 32], AT.isVariadic = False})

    it "parses int int -> void" $ do
      parseWithEnv "(int int) -> (void)" `shouldBe` Right (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 32, AT.TInt 32], AT.isVariadic = False})

  describe "Custom Types" $ do
    it "parses a defined custom struct type" $ do
      let env = E.insertType "Point" (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]) E.emptyEnv
      parseWithCustomEnv "Point" env `shouldBe` Right (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)])

    it "parses a defined custom alias type" $ do
      let env = E.insertType "Vector2" (AT.TTypedef "Vector2" (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)])) E.emptyEnv
      parseWithCustomEnv "Vector2" env `shouldBe` Right (AT.TTypedef "Vector2" (AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]))

    it "returns an error for an undefined custom type" $ do
      let env = E.emptyEnv
      parseWithCustomEnv "UnknownType" env `shouldSatisfy` isLeft

    it "parses nested custom types" $ do
      let pointType = AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]
      let env =
            E.insertType "Point" pointType $
              E.insertType "Shape" (AT.TStruct "Shape" [("center", pointType)]) E.emptyEnv
      parseWithCustomEnv "Shape" env `shouldBe` Right (AT.TStruct "Shape" [("center", pointType)])
