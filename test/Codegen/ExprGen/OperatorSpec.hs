module Codegen.ExprGen.OperatorSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Codegen.Errors as CE
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Operator" $ do
  let wrapInFunction expr =
        AT.Function
          { AT.funcLoc = sampleLoc,
            AT.funcName = "test",
            AT.funcType = AT.TFunction AT.TVoid [] False,
            AT.funcParams = [],
            AT.funcBody =
              expr
          }
  H.describe "generateBinaryOp" $ do
    H.it "should generate binary operator (Pointer -> Integer)" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                  AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                  AT.Op sampleLoc AT.Add (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Sub (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))) (AT.Lit sampleLoc (AT.LInt 0))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1

      case L.find isGepInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          i `H.shouldBe` [AST.ConstantOperand (C.Int 32 0)]
        _ -> H.expectationFailure "Expected a GetElementPtr instruction"

      case drop 1 (filter isGepInstr instrs) of
        (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) : _ -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          map TD.typeOf i `H.shouldBe` [T.IntegerType 32]
        _ -> H.expectationFailure "Expected a second GetElementPtr instruction"

      case L.find isLoadInstr instrs of
        Just (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
        _ -> H.expectationFailure "Expected a Load instruction"

      case drop 1 (filter isLoadInstr instrs) of
        (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
          al `H.shouldBe` 0
        _ -> H.expectationFailure "Expected a second Load instruction"

      case L.find isSubInstr instrs of
        Just (AST.UnName _ AST.:= AST.Sub {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a Sub instruction"

    H.it "should throw an error for unsupported binary operator (Pointer -> Integer)" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                  AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                  AT.Op sampleLoc AT.Mul (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))) (AT.Lit sampleLoc (AT.LInt 0))
                ]
          expectedError = CE.CodegenError sampleLoc (CE.UnsupportedOperator AT.Mul)
      testError expectedError funcExpr
  where
    -- H.it "should generate binary operator (Float -> Float)" $ do
    --   let funcExpr =
    --         wrapInFunction $
    --           AT.Block
    --             [
    --               AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
    --               AT.Op sampleLoc AT.Add (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
    --               AT.Op sampleLoc AT.Sub (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
    --               AT.Op sampleLoc AT.Mul (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
    --               AT.Op sampleLoc AT.Div (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
    --               AT.Op sampleLoc AT.Mod (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0))
    --             ]
    --   let blocks = generateTestBlocks funcExpr
    --   let instrs = getInstructions blocks

    --   length blocks `H.shouldBe` 1

    --   case L.find isGepInstr instrs of
    --     Just (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) -> do
    --       TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
    --       i `H.shouldBe` [AST.ConstantOperand (C.Int 32 0)]
    --     _ -> H.expectationFailure "Expected a GetElementPtr instruction"

    --   case drop 1 (filter isGepInstr instrs) of
    --     (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) : _ -> do
    --       TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
    --       map TD.typeOf i `H.shouldBe` [T.IntegerType 32]
    --     _ -> H.expectationFailure "Expected a second GetElementPtr instruction"

    --   case L.find isLoadInstr instrs of
    --     Just (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v}) -> do
    --       TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
    --       v `H.shouldBe` False
    --     _ -> H.expectationFailure "Expected a Load instruction"

    --   case drop 1 (filter isLoadInstr instrs) of
    --     (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
    --       TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
    --       v `H.shouldBe` False
    --       al `H.shouldBe` 0
    --     _ -> H.expectationFailure "Expected a second Load instruction"

    --   case L.find isSubInstr instrs of
    --     Just (AST.UnName _ AST.:= AST.Sub {AST.operand0 = o0, AST.operand1 = o1}) -> do
    --       TD.typeOf o0 `H.shouldBe` T.IntegerType 32
    --       TD.typeOf o1 `H.shouldBe` T.IntegerType 32
    --     _ -> H.expectationFailure "Expected a Sub instruction"

    sampleLoc = AT.SrcLoc "test.c" 1 1

    testError :: CE.CodegenError -> AT.Expr -> H.Expectation
    testError expectedError expr = do
      let testProg = AT.Program [("test", expr)] [] "test.c"
      let result = CC.codegen testProg
      case result of
        Left error' -> do
          error' `H.shouldBe` expectedError
        Right _ -> H.expectationFailure "Expected a CodegenError, but codegen succeeded"

    generateTestBlocks expr = case CC.codegen testProg of
      Right mod' -> concatMap G.basicBlocks $ getDefinitions mod'
      Left error' -> error $ show error'
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']

    getInstructions blocks =
      [i | G.BasicBlock _ instrs _ <- blocks, i <- instrs]

    isGepInstr (AST.UnName _ AST.:= AST.GetElementPtr {}) = True
    isGepInstr _ = False

    isLoadInstr (AST.UnName _ AST.:= AST.Load {}) = True
    isLoadInstr _ = False

    isSubInstr (AST.UnName _ AST.:= AST.Sub {}) = True
    isSubInstr _ = False

    isAddInstr (AST.UnName _ AST.:= AST.Add {}) = True
    isAddInstr _ = False

    isMulInstr (AST.UnName _ AST.:= AST.Mul {}) = True
    isMulInstr _ = False

    isDivInstr (AST.UnName _ AST.:= AST.SDiv {}) = True
    isDivInstr _ = False

    isModInstr (AST.UnName _ AST.:= AST.SRem {}) = True
    isModInstr _ = False

    isBitAndInstr (AST.UnName _ AST.:= AST.And {}) = True
    isBitAndInstr _ = False

    isBitOrInstr (AST.UnName _ AST.:= AST.Or {}) = True
    isBitOrInstr _ = False

    isBitXorInstr (AST.UnName _ AST.:= AST.Xor {}) = True
    isBitXorInstr _ = False

    isBitShlInstr (AST.UnName _ AST.:= AST.Shl {}) = True
    isBitShlInstr _ = False

    isBitShrInstr (AST.UnName _ AST.:= AST.LShr {}) = True
    isBitShrInstr _ = False

    isFAddInstr (AST.UnName _ AST.:= AST.FAdd {}) = True
    isFAddInstr _ = False

    isFSubInstr (AST.UnName _ AST.:= AST.FSub {}) = True
    isFSubInstr _ = False

    isFMulInstr (AST.UnName _ AST.:= AST.FMul {}) = True
    isFMulInstr _ = False

    isFDivInstr (AST.UnName _ AST.:= AST.FDiv {}) = True
    isFDivInstr _ = False
