{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen.ExprGen.StateSpec where

import qualified Codegen.State as CS
import qualified Codegen.Utils as CU
import qualified Control.Monad.State as S
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "Codegen.State" $ do
  let initialState =
        CS.CodegenState
          { CS.localState = [("x", dummyOperand "x")],
            CS.globalState = [("y", dummyOperand "y")],
            CS.loopState = Nothing,
            CS.allocatedVars = [("z", dummyOperand "z")],
            CS.uniqueNameState = 0
          }

  H.describe "getVar" $ do
    H.it "returns a variable from allocatedVars" $ do
      S.evalState (CS.getVar "z") initialState `H.shouldBe` Just (dummyOperand "z")

    H.it "returns a variable from localState" $ do
      S.evalState (CS.getVar "x") initialState `H.shouldBe` Just (dummyOperand "x")

    H.it "returns a variable from globalState" $ do
      S.evalState (CS.getVar "y") initialState `H.shouldBe` Just (dummyOperand "y")

    H.it "returns Nothing for an unknown variable" $ do
      S.evalState (CS.getVar "unknown") initialState `H.shouldBe` Nothing

  H.describe "addVar" $ do
    H.it "adds a variable to localState" $ do
      let updatedState = S.execState (CS.addVar "a" (dummyOperand "a")) initialState
      CS.localState updatedState `H.shouldBe` [("a", dummyOperand "a"), ("x", dummyOperand "x")]

  H.describe "getGlobalVar" $ do
    H.it "returns a global variable from globalState" $ do
      S.evalState (CS.getGlobalVar "y") initialState `H.shouldBe` Just (dummyOperand "y")

    H.it "returns Nothing for an unknown global variable" $ do
      S.evalState (CS.getGlobalVar "unknown") initialState `H.shouldBe` Nothing

  H.describe "addGlobalVar" $ do
    H.it "adds a global variable to globalState" $ do
      let updatedState = S.execState (CS.addGlobalVar "b" (dummyOperand "b")) initialState
      CS.globalState updatedState `H.shouldBe` [("b", dummyOperand "b"), ("y", dummyOperand "y")]

  H.describe "fresh" $ do
    H.it "generates a fresh unique name" $ do
      let (name, updatedState) = S.runState CS.fresh initialState
      name `H.shouldBe` AST.Name (CU.stringToByteString "_0")
      CS.uniqueNameState updatedState `H.shouldBe` 1

  H.describe "freshName" $ do
    H.it "generates a fresh unique name with a given prefix" $ do
      let (name, updatedState) = S.runState (CS.freshName "prefix_") initialState
      name `H.shouldBe` AST.Name (CU.stringToByteString "prefix_0")
      CS.uniqueNameState updatedState `H.shouldBe` 1

-- Helper function to create a dummy Operand
dummyOperand :: String -> AST.Operand
dummyOperand name = AST.LocalReference (AST.PointerType (AST.NamedTypeReference "dummy") (AS.AddrSpace 0)) (AST.Name (CU.stringToByteString name))
