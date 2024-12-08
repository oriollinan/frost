{-# LANGUAGE OverloadedStrings #-}

module Codegen.CodegenSpec (spec) where

import Codegen.Codegen
import Control.Monad.Identity (runIdentity)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.IRBuilder.Monad as IRM
import Test.Hspec

spec :: Spec
spec = do
  describe "generateExpr" $ do
    it "handles literal expressions correctly" $ do
      let input = AT.Lit (AT.LInt 42)
      evalMonadCodegen (generateExpr input) `shouldBe` Right (AST.ConstantOperand $ C.Int 32 42)

    it "handles binary operations correctly" $ do
      let input = AT.Op AT.Add (AT.Lit (AT.LInt 10)) (AT.Lit (AT.LInt 32))
      pending

    it "handles if expressions correctly" $ do
      let input = AT.If (AT.Lit (AT.LBool True)) (AT.Lit (AT.LInt 1)) (AT.Lit (AT.LInt 0))
      pending

    it "handles function calls correctly" $ do
      let input = AT.Call (AT.Var "someFunction") (AT.Lit (AT.LInt 42))
      pending

    it "handles variable expressions correctly" $ do
      let state = [("x", dummyOperand)]
      let input = AT.Var "x"
      evalMonadCodegenWithState (generateExpr input) state `shouldBe` Right dummyOperand

    it "handles definitions correctly" $ do
      let input = AT.Define "x" (AT.Lit (AT.LInt 42))
      pending

    it "handles lambda expressions correctly" $ do
      let input = AT.Lambda ["x"] (AT.Lit (AT.LInt 42))
      pending

dummyOperand :: AST.Operand
dummyOperand = AST.ConstantOperand (C.Int 32 42)

evalMonadCodegen :: E.ExceptT CodegenError (S.State CodegenState) a -> Either CodegenError a
evalMonadCodegen action = S.evalState (E.runExceptT action) []

evalMonadCodegenWithState :: E.ExceptT CodegenError (S.State CodegenState) a -> CodegenState -> Either CodegenError a

evalMonadCodegenWithState action
