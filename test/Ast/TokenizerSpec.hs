module Ast.TokenizerSpec (spec) where

import Ast.Tokenizer (Literal (..), Separator (..), Token (..), tokenize)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Ast.Tokenizer" $ do
    it "parses a simple number" $ do
      tokenize "42" `shouldBe` Right [TLiteral (LNumber 42)]

    it "parses a boolean" $ do
      tokenize "#t" `shouldBe` Right [TLiteral (LBoolean True)]

    it "parses symbols" $ do
      tokenize "foo" `shouldBe` Right [TLiteral (LSymbol "foo")]

    it "parses parentheses" $ do
      tokenize "()" `shouldBe` Right [TSeparator OpenParen, TSeparator CloseParen]

    it "handles mixed input" $ do
      tokenize "(+ 1 2)"
        `shouldBe` Right [TSeparator OpenParen, TLiteral (LSymbol "+"), TLiteral (LNumber 1), TLiteral (LNumber 2), TSeparator CloseParen]
