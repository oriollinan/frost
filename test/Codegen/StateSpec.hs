{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.StateSpec (spec) where

import qualified Codegen.State as CS
import qualified Control.Monad.State as S
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "Codegen.State" $ do
  H.describe "VarBinding" $ do
    H.it "should add and get local variables" $ do
      let initialState = emptyState
          op = AST.ConstantOperand (C.Int 32 42)
          result = S.evalState (testAddGetVar "x" op) initialState
      result `H.shouldBe` Just op

    H.it "should add and get global variables" $ do
      let initialState = emptyState
          op = AST.ConstantOperand (C.Int 32 42)
          result = S.evalState (testAddGetGlobalVar "x" op) initialState
      result `H.shouldBe` Just op

    H.it "should return Nothing for non-existent variables" $ do
      let result = S.evalState (CS.getVar "nonexistent") emptyState
      result `H.shouldBe` Nothing

    H.it "should handle multiple variables" $ do
      let initialState = emptyState
          op1 = AST.ConstantOperand (C.Int 32 42)
          op2 = AST.ConstantOperand (C.Int 32 24)
          result = S.evalState (testMultipleVars op1 op2) initialState
      result `H.shouldBe` (Just op1, Just op2)

  H.describe "fresh" $ do
    H.it "should generate unique names" $ do
      let result = S.evalState testFreshNames emptyState
      result `H.shouldBe` [AST.Name "_0", AST.Name "_1", AST.Name "_2"]

  H.describe "freshName" $ do
    H.it "should generate unique names with prefix" $ do
      let result = S.evalState testFreshNameWithPrefix emptyState
      result `H.shouldBe` [AST.Name "test0", AST.Name "test1", AST.Name "foo2"]
  where
    emptyState =
      CS.CodegenState
        { CS.localState = [],
          CS.globalState = [],
          CS.loopState = Nothing,
          CS.allocatedVars = [],
          CS.uniqueNameState = 0
        }

    testAddGetVar :: String -> AST.Operand -> S.State CS.CodegenState (Maybe AST.Operand)
    testAddGetVar name op = do
      CS.addVar name op
      CS.getVar name

    testAddGetGlobalVar :: String -> AST.Operand -> S.State CS.CodegenState (Maybe AST.Operand)
    testAddGetGlobalVar name op = do
      CS.addGlobalVar name op
      CS.getGlobalVar name

    testMultipleVars :: AST.Operand -> AST.Operand -> S.State CS.CodegenState (Maybe AST.Operand, Maybe AST.Operand)
    testMultipleVars op1 op2 = do
      CS.addVar "x" op1
      CS.addVar "y" op2
      x <- CS.getVar "x"
      y <- CS.getVar "y"
      return (x, y)

    testFreshNames :: S.State CS.CodegenState [AST.Name]
    testFreshNames = do
      name1 <- CS.fresh
      name2 <- CS.fresh
      name3 <- CS.fresh
      return [name1, name2, name3]

    testFreshNameWithPrefix :: S.State CS.CodegenState [AST.Name]
    testFreshNameWithPrefix = do
      name1 <- CS.freshName "test"
      name2 <- CS.freshName "test"
      name3 <- CS.freshName "foo"
      return [name1, name2, name3]
