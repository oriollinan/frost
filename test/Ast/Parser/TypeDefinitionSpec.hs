module Ast.Parser.TypeDefinitionSpec (spec) where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.TypeDefinition as PT
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parse input = do
        (result, _) <- S.runStateT (M.runParserT PT.parseTypeDefinition "" input) PS.parserState
        return result
  let parseMany input = do
        (result, _) <- S.runStateT (M.runParserT (M.many PT.parseTypeDefinition) "" input) PS.parserState
        return result

  describe "Struct Types" $ do
    it "parses struct { name -> int }" $ do
      let input = "test :: struct { name -> int }"
      result <- parse input
      let expected = Right (AT.TStruct {AT.structName = "test", AT.fields = [("name", AT.TInt 32)]})
      result `shouldBe` expected

    it "parses struct with multiple fields" $ do
      let input = "vec :: struct { x -> float y -> float }"
      result <- parse input
      let expected = Right (AT.TStruct {AT.structName = "vec", AT.fields = [("x", AT.TFloat), ("y", AT.TFloat)]})
      result `shouldBe` expected

  describe "Union Types" $ do
    it "parses union { name -> int }" $ do
      let input = "test :: union { name -> int }"
      result <- parse input
      let expected = Right (AT.TUnion {AT.unionName = "test", AT.variants = [("name", AT.TInt 32)]})
      result `shouldBe` expected

    it "parses union with multiple fields" $ do
      let input = "vec :: union { x -> float y -> float }"
      result <- parse input
      let expected = Right (AT.TUnion {AT.unionName = "vec", AT.variants = [("x", AT.TFloat), ("y", AT.TFloat)]})
      result `shouldBe` expected

  describe "Typedefs" $ do
    it "parses typedef alias" $ do
      let input = "Alias :: int"
      result <- parse input
      let expected = Right (AT.TTypedef "Alias" (AT.TInt 32))
      result `shouldBe` expected

    it "parses typedef for a function" $ do
      let input = "Alias :: int -> char"
      result <- parse input
      let expected = Right (AT.TTypedef "Alias" (AT.TFunction AT.TChar [AT.TInt 32] False))
      result `shouldBe` expected

    it "parses typedef and then a struct" $ do
      let input = "bestInt :: int69 vec :: struct { x -> int y -> int }"
      result <- parseMany input
      let expected = Right [AT.TTypedef "bestInt" $ AT.TInt 69, AT.TStruct "vec" [("x", AT.TInt 32), ("y", AT.TInt 32)]]
      result `shouldBe` expected
