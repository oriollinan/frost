module Ast.Parser.TypeDefinitionSpec (spec) where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.TypeDefinition as P
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parseWithEnv input =
        fst $ S.runState (M.runParserT P.parseTypeDefinition "" input) E.emptyEnv

  describe "Struct Types" $ do
    it "parses struct { name -> int }" $ do
      parseWithEnv "test :: struct { name -> int }" `shouldBe` Right ()

    it "parses struct with multiple fields" $ do
      parseWithEnv "vec :: struct { x -> float y -> float }" `shouldBe` Right ()

  describe "Union Types" $ do
    it "parses union { name -> int }" $ do
      parseWithEnv "test :: union { name -> int }" `shouldBe` Right ()

    it "parses union with multiple fields" $ do
      parseWithEnv "vec :: union { x -> float y -> float }" `shouldBe` Right ()

  describe "Typedefs" $ do
    it "parses typedef alias" $ do
      parseWithEnv "Alias :: int" `shouldBe` Right ()
