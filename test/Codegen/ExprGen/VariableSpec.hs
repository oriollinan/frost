{-# LANGUAGE OverloadedStrings #-}

module Codegen.ExprGen.VariableSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Variable" $ do
  H.describe "Variable Declaration and Initialization" $ do
    H.it "should generate declaration with initialization" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration
                    sampleLoc
                    "x"
                    (AT.TInt 32)
                    (Just (AT.Lit sampleLoc (AT.LInt 42)))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isStoreInstr instrs of
        Just (AST.Do AST.Store {AST.value = val}) ->
          val `H.shouldBe` AST.ConstantOperand (C.Int 32 42)
        _ -> H.expectationFailure "Expected a store instruction"

    H.it "should handle empty declarations" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block [AT.Declaration sampleLoc "x" (AT.TInt 32) Nothing]
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate string declaration" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration
                    sampleLoc
                    "str"
                    (AT.TArray AT.TChar Nothing)
                    (Just (AT.Lit sampleLoc (AT.LArray [AT.LChar 'h', AT.LChar 'i'])))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isGlobalStringPtr instrs of
        Just (AST.UnName _ AST.:= AST.Load {}) -> return ()
        _ -> H.expectationFailure "Expected a global string pointer"

  H.describe "Struct Operations" $ do
    H.it "should handle nested struct access" $ do
      let innerType = AT.TStruct "Inner" [("val", AT.TInt 32)]
      let outerType = AT.TStruct "Outer" [("inner", innerType)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "obj" outerType Nothing,
                  AT.StructAccess
                    sampleLoc
                    ( AT.StructAccess
                        sampleLoc
                        (AT.Var sampleLoc "obj" outerType)
                        (AT.Var sampleLoc "inner" innerType)
                    )
                    (AT.Var sampleLoc "val" (AT.TInt 32))
                ]
      let blocks = generateTestBlocks funcExpr
      case filter isGEPInstr (getInstructions blocks) of
        (_ : _ : _) -> return ()
        _ -> H.expectationFailure "Expected multiple GEP instructions for nested struct"

    H.it "should generate struct field access" $ do
      let structType = AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "p" structType Nothing,
                  AT.StructAccess
                    sampleLoc
                    (AT.Var sampleLoc "p" structType)
                    (AT.Var sampleLoc "x" (AT.TInt 32))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isGEPInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {}) -> return ()
        _ -> H.expectationFailure "Expected a GEP instruction for struct access"

  H.describe "Unary Operations" $ do
    H.it "should generate negation" $ do
      let funcExpr =
            wrapInFunction $
              AT.UnaryOp
                sampleLoc
                AT.Not
                (AT.Lit sampleLoc (AT.LInt 42))

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isNegInstr instrs of
        Just (AST.UnName _ AST.:= AST.Xor {}) -> return ()
        _ -> H.expectationFailure "Expected a negation instruction"

    H.it "should generate logical not" $ do
      let funcExpr =
            wrapInFunction $
              AT.UnaryOp
                sampleLoc
                AT.Not
                (AT.Lit sampleLoc (AT.LBool True))

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isNotInstr instrs of
        Just (AST.UnName _ AST.:= AST.Xor {}) -> return ()
        _ -> H.expectationFailure "Expected a logical not instruction"

  H.describe "Assignment Operations" $ do
    H.it "should generate simple variable assignment" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "x" (AT.TInt 32) Nothing,
                  AT.Assignment
                    sampleLoc
                    (AT.Var sampleLoc "x" (AT.TInt 32))
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isStoreInstr instrs of
        Just (AST.Do AST.Store {AST.value = val}) ->
          val `H.shouldBe` AST.ConstantOperand (C.Int 32 42)
        _ -> H.expectationFailure "Expected a store instruction"

    H.it "should generate array element assignment" $ do
      let arrayType = AT.TArray (AT.TInt 32) (Just 10)
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "arr" arrayType Nothing,
                  AT.Assignment
                    sampleLoc
                    ( AT.ArrayAccess
                        sampleLoc
                        (AT.Var sampleLoc "arr" arrayType)
                        (AT.Lit sampleLoc (AT.LInt 0))
                    )
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      case filter isGEPInstr instrs of
        (_ : _) -> return ()
        _ -> H.expectationFailure "Expected GEP instruction for array access"

  H.describe "Array Operations" $ do
    H.it "should generate multi-dimensional array access" $ do
      let arrayType = AT.TArray (AT.TArray (AT.TInt 32) (Just 10)) (Just 10)
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "arr" arrayType Nothing,
                  AT.ArrayAccess
                    sampleLoc
                    ( AT.ArrayAccess
                        sampleLoc
                        (AT.Var sampleLoc "arr" arrayType)
                        (AT.Lit sampleLoc (AT.LInt 1))
                    )
                    (AT.Lit sampleLoc (AT.LInt 2))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case filter isGEPInstr instrs of
        (_ : _ : _) -> return ()
        _ -> H.expectationFailure "Expected multiple GEP instructions for multi-dimensional array"

  H.describe "generateAssignment" $ do
    H.it "should generate simple variable assignment" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "x" (AT.TInt 32) Nothing,
                  AT.Assignment
                    sampleLoc
                    (AT.Var sampleLoc "x" (AT.TInt 32))
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isStoreInstr instrs of
        Just (AST.Do AST.Store {AST.value = val}) ->
          val `H.shouldBe` AST.ConstantOperand (C.Int 32 42)
        _ -> H.expectationFailure "Expected a store instruction"

    H.it "should generate array element assignment" $ do
      let arrayType = AT.TArray (AT.TInt 32) (Just 10)
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "arr" arrayType Nothing,
                  AT.Assignment
                    sampleLoc
                    ( AT.ArrayAccess
                        sampleLoc
                        (AT.Var sampleLoc "arr" arrayType)
                        (AT.Lit sampleLoc (AT.LInt 0))
                    )
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      case filter isGEPInstr instrs of
        (_ : _) -> return ()
        _ -> H.expectationFailure "Expected GEP instruction for array access"

    H.it "should generate pointer dereference assignment" $ do
      let ptrType = AT.TPointer (AT.TInt 32)
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "ptr" ptrType Nothing,
                  AT.Assignment
                    sampleLoc
                    ( AT.UnaryOp
                        sampleLoc
                        AT.Deref
                        (AT.Var sampleLoc "ptr" ptrType)
                    )
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      case L.find isLoadInstr instrs of
        Just _ -> return ()
        _ -> H.expectationFailure "Expected load instruction for pointer dereference"

    H.it "should generate struct field assignment" $ do
      let structType = AT.TStruct "Point" [("x", AT.TInt 32), ("y", AT.TInt 32)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "p" structType Nothing,
                  AT.Assignment
                    sampleLoc
                    ( AT.StructAccess
                        sampleLoc
                        (AT.Var sampleLoc "p" structType)
                        (AT.Var sampleLoc "x" (AT.TInt 32))
                    )
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      case L.find isGEPInstr instrs of
        Just _ -> return ()
        _ -> H.expectationFailure "Expected GEP instruction for struct field access"
  where
    sampleLoc = AT.SrcLoc "test.c" 1 1

    wrapInFunction expr =
      AT.Function
        { AT.funcLoc = sampleLoc,
          AT.funcName = "test",
          AT.funcType = AT.TFunction AT.TVoid [] False,
          AT.funcParams = [],
          AT.funcBody = expr
        }

    generateTestBlocks expr = case CC.codegen testProg of
      Right mod' -> concatMap G.basicBlocks $ getDefinitions mod'
      Left _ -> []
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']

    getInstructions blocks =
      [i | G.BasicBlock _ instrs _ <- blocks, i <- instrs]

    isStoreInstr (AST.Do AST.Store {}) = True
    isStoreInstr _ = False

    isGEPInstr (AST.UnName _ AST.:= AST.GetElementPtr {}) = True
    isGEPInstr _ = False

    isGlobalStringPtr (AST.UnName _ AST.:= AST.Load {AST.address = AST.LocalReference _ _}) = True
    isGlobalStringPtr _ = False

    isNegInstr (AST.UnName _ AST.:= AST.Xor {}) = True
    isNegInstr _ = False

    isNotInstr (AST.UnName _ AST.:= AST.Xor {}) = True
    isNotInstr _ = False

    isLoadInstr (AST.UnName _ AST.:= AST.Load {}) = True
    isLoadInstr _ = False
