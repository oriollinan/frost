module Ast.TypesSpec (spec) where

import Ast.Types
  ( AST (..),
    Expr (..),
    Literal (..),
    Operation (..),
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "Literal instances" $ do
    it "tests the Show instance for Literal" $ do
      show (LInt 42) `shouldBe` "LInt 42"
      show (LBool True) `shouldBe` "LBool True"
      show (LSymbol "x") `shouldBe` "LSymbol \"x\""

    it "tests the Eq instance for Literal" $ do
      LInt 42 `shouldBe` LInt 42
      LBool False `shouldBe` LBool False
      LSymbol "x" `shouldBe` LSymbol "x"
      LInt 1 `shouldNotBe` LInt 2
      LBool True `shouldNotBe` LBool False

    it "tests the Ord instance for Literal" $ do
      LInt 1 `shouldSatisfy` (< LInt 2)
      LBool False `shouldSatisfy` (< LBool True)
      LSymbol "a" `shouldSatisfy` (< LSymbol "b")

  describe "Expr instances" $ do
    it "tests the Show instance for Expr" $ do
      show (Lit (LInt 42)) `shouldBe` "Lit (LInt 42)"
      show (Var "x") `shouldBe` "Var \"x\""
      show (Define "x" (Lit (LInt 10))) `shouldBe` "Define \"x\" (Lit (LInt 10))"

    it "tests the Eq instance for Expr" $ do
      Lit (LInt 42) `shouldBe` Lit (LInt 42)
      Var "x" `shouldBe` Var "x"
      Define "x" (Lit (LInt 10)) `shouldBe` Define "x" (Lit (LInt 10))
      Lit (LInt 1) `shouldNotBe` Lit (LInt 2)

    it "tests the Ord instance for Expr" $ do
      Lit (LInt 1) `shouldSatisfy` (< Lit (LInt 2))
      Var "a" `shouldSatisfy` (< Var "b")
      Define "a" (Lit (LInt 10)) `shouldSatisfy` (< Define "b" (Lit (LInt 20)))

  describe "Operation instances" $ do
    it "tests the Show instance for Operation" $ do
      show Add `shouldBe` "Add"
      show Sub `shouldBe` "Sub"
      show Mult `shouldBe` "Mult"

    it "tests the Eq instance for Operation" $ do
      Add `shouldBe` Add
      Sub `shouldBe` Sub
      Mult `shouldNotBe` Div

    it "tests the Ord instance for Operation" $ do
      Add `shouldSatisfy` (< Sub)
      Sub `shouldSatisfy` (< Mult)
      Div `shouldSatisfy` (< Mod)

  describe "AST instances" $ do
    it "tests the Show instance for AST" $ do
      show (AST [Lit (LInt 42)]) `shouldBe` "AST [Lit (LInt 42)]"
      show (AST [Var "x", Lit (LBool True)]) `shouldBe` "AST [Var \"x\",Lit (LBool True)]"

    it "tests the Eq instance for AST" $ do
      AST [Lit (LInt 42)] `shouldBe` AST [Lit (LInt 42)]
      AST [Var "x", Lit (LBool True)] `shouldNotBe` AST [Var "x"]
