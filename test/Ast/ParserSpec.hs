module Ast.ParserSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "addOne" $ do
    it "adds one to a number" $ do
      1 + 1 `shouldBe` 2
