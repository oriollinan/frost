module Ast.Parser.TypeDefinitionSpec (spec) where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.TypeDefinition as PT
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parseWithEnv input =
        fst $ S.runState (M.runParserT PT.parseTypeDefinition "" input) PS.parserState

  describe "Struct Types" $ do
    it "parses struct { name -> int }" $ do
      parseWithEnv "test :: struct { name -> int }" `shouldBe` Right (AT.TStruct {AT.structName = "test", AT.fields = [("name", AT.TInt 32)]})

    it "parses struct with multiple fields" $ do
      parseWithEnv "vec :: struct { x -> float y -> float }" `shouldBe` Right (AT.TStruct {AT.structName = "vec", AT.fields = [("x", AT.TFloat), ("y", AT.TFloat)]})

  describe "Union Types" $ do
    it "parses union { name -> int }" $ do
      parseWithEnv "test :: union { name -> int }" `shouldBe` Right (AT.TUnion {AT.unionName = "test", AT.variants = [("name", AT.TInt 32)]})

    it "parses union with multiple fields" $ do
      parseWithEnv "vec :: union { x -> float y -> float }" `shouldBe` Right (AT.TUnion {AT.unionName = "vec", AT.variants = [("x", AT.TFloat), ("y", AT.TFloat)]})

  describe "Typedefs" $ do
    it "parses typedef alias" $ do
      parseWithEnv "Alias :: int" `shouldBe` Right (AT.TTypedef "Alias" (AT.TInt 32))

    it "parses typedef for a function" $ do
      parseWithEnv "Alias :: int -> char" `shouldBe` Right (AT.TTypedef "Alias" (AT.TFunction AT.TChar [AT.TInt 32] False))

    it "parses typedef and then a struct" $ do
      let input = "bestInt :: int69 vec :: struct { x -> int y -> int }"
      let result = fst $ S.runState (M.runParserT (M.some $ PU.lexeme PT.parseTypeDefinition) "" input) PS.parserState
      let expected = Right [AT.TTypedef "bestInt" $ AT.TInt 69, AT.TStruct "vec" [("x", AT.TInt 32), ("y", AT.TInt 32)]]
      result `shouldBe` expected
