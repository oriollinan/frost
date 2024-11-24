module Misc.MiscSpec (spec) where

import Misc (addOne)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "addOne" $ do
    it "adds one to a number" $ do
      addOne 1 `shouldBe` 2
    it "adds one to a negative number" $ do
      addOne (-1) `shouldBe` 0
    it "is the inverse of subtracting one" $
      property $
        \x -> addOne (x - 1) == x
