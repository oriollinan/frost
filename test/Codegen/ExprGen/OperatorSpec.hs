module Codegen.ExprGen.OperatorSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Codegen.Errors as CE
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.IntegerPredicate as IP
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

    H.it "should generate binary operator (Integer -> Integer)" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                  AT.Op sampleLoc AT.Add (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Sub (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Mul (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Div (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Mod (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.BitAnd (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.BitOr (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.BitXor (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.BitShl (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.BitShr (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.And (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Or (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Lt (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Gt (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Lte (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Gte (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Eq (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0)),
                  AT.Op sampleLoc AT.Ne (AT.Var sampleLoc "myInt" (AT.TInt 32)) (AT.Lit sampleLoc (AT.LInt 0))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1

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

      case L.find isAddInstr instrs of
        Just (AST.UnName _ AST.:= AST.Add {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a FAdd instruction"

      case L.find isSubInstr instrs of
        Just (AST.UnName _ AST.:= AST.Sub {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a Sub instruction"

      case L.find isMulInstr instrs of
        Just (AST.UnName _ AST.:= AST.Mul {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a Mul instruction"

      case L.find isDivInstr instrs of
        Just (AST.UnName _ AST.:= AST.SDiv {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a Div instruction"

      case L.find isModInstr instrs of
        Just (AST.UnName _ AST.:= AST.SRem {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a Mod instruction"

      case L.find isBitAndInstr instrs of
        Just (AST.UnName _ AST.:= AST.And {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a BitAnd instruction"

      case L.find isBitOrInstr instrs of
        Just (AST.UnName _ AST.:= AST.Or {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a BitOr instruction"

      case L.find isBitXorInstr instrs of
        Just (AST.UnName _ AST.:= AST.Xor {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a BitXor instruction"

      case L.find isBitShlInstr instrs of
        Just (AST.UnName _ AST.:= AST.Shl {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a BitShl instruction"

      case L.find isBitShrInstr instrs of
        Just (AST.UnName _ AST.:= AST.AShr {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a BitShr instruction"

      case L.find isAndInstr instrs of
        Just (AST.UnName _ AST.:= AST.And {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a And instruction"

      case L.find isOrInstr instrs of
        Just (AST.UnName _ AST.:= AST.Or {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a Or instruction"

      case L.find isICmpInstr instrs of
        Just (AST.UnName _ AST.:= AST.ICmp {AST.iPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) -> do
          p `H.shouldBe` IP.SLT
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a FCmp instruction"

      case drop 1 (filter isICmpInstr instrs) of
        (AST.UnName _ AST.:= AST.ICmp {AST.iPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` IP.SGT
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a second FCmp instruction"

      case drop 2 (filter isICmpInstr instrs) of
        (AST.UnName _ AST.:= AST.ICmp {AST.iPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` IP.SLE
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a third FCmp instruction"

      case drop 3 (filter isICmpInstr instrs) of
        (AST.UnName _ AST.:= AST.ICmp {AST.iPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` IP.SGE
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a fourth FCmp instruction"

      case drop 4 (filter isICmpInstr instrs) of
        (AST.UnName _ AST.:= AST.ICmp {AST.iPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` IP.EQ
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a fifth FCmp instruction"

      case drop 5 (filter isICmpInstr instrs) of
        (AST.UnName _ AST.:= AST.ICmp {AST.iPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` IP.NE
          TD.typeOf o0 `H.shouldBe` T.IntegerType 32
          TD.typeOf o1 `H.shouldBe` T.IntegerType 32
        _ -> H.expectationFailure "Expected a sixth FCmp instruction"

    H.it "should generate binary operator (Float -> Float)" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                  AT.Op sampleLoc AT.Add (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Sub (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Mul (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Div (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Mod (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Lt (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Gt (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Lte (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Gte (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Eq (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0)),
                  AT.Op sampleLoc AT.Ne (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1

      case L.find isLoadInstr instrs of
        Just (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.FloatingPointType T.FloatFP, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
        _ -> H.expectationFailure "Expected a Load instruction"

      case drop 1 (filter isLoadInstr instrs) of
        (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.FloatingPointType T.FloatFP, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
          al `H.shouldBe` 0
        _ -> H.expectationFailure "Expected a second Load instruction"

      case L.find isFAddInstr instrs of
        Just (AST.UnName _ AST.:= AST.FAdd {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a FAdd instruction"

      case L.find isFSubInstr instrs of
        Just (AST.UnName _ AST.:= AST.FSub {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a Sub instruction"

      case L.find isFMulInstr instrs of
        Just (AST.UnName _ AST.:= AST.FMul {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a Mul instruction"

      case L.find isFDivInstr instrs of
        Just (AST.UnName _ AST.:= AST.FDiv {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a Div instruction"

      case L.find isFModInstr instrs of
        Just (AST.UnName _ AST.:= AST.FRem {AST.operand0 = o0, AST.operand1 = o1}) -> do
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a Mod instruction"

      case L.find isFmcpInstr instrs of
        Just (AST.UnName _ AST.:= AST.FCmp {AST.fpPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) -> do
          p `H.shouldBe` FP.OLT
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a FCmp instruction"

      case drop 1 (filter isFmcpInstr instrs) of
        (AST.UnName _ AST.:= AST.FCmp {AST.fpPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` FP.OGT
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a second FCmp instruction"

      case drop 2 (filter isFmcpInstr instrs) of
        (AST.UnName _ AST.:= AST.FCmp {AST.fpPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` FP.OLE
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a third FCmp instruction"

      case drop 3 (filter isFmcpInstr instrs) of
        (AST.UnName _ AST.:= AST.FCmp {AST.fpPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` FP.OGE
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a fourth FCmp instruction"

      case drop 4 (filter isFmcpInstr instrs) of
        (AST.UnName _ AST.:= AST.FCmp {AST.fpPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` FP.OEQ
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a fifth FCmp instruction"

      case drop 5 (filter isFmcpInstr instrs) of
        (AST.UnName _ AST.:= AST.FCmp {AST.fpPredicate = p, AST.operand0 = o0, AST.operand1 = o1}) : _ -> do
          p `H.shouldBe` FP.ONE
          TD.typeOf o0 `H.shouldBe` T.FloatingPointType T.FloatFP
          TD.typeOf o1 `H.shouldBe` T.FloatingPointType T.FloatFP
        _ -> H.expectationFailure "Expected a sixth FCmp instruction"

    H.it "should throw an error for unsupported binary operator (Float -> Float)" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                  AT.Op sampleLoc AT.BitXor (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Lit sampleLoc (AT.LFloat 0.0))
                ]
      let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedOperator AT.BitXor)
      testError expectedError funcExpr

    H.it "should theow an error for unsupported binary operator type" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                  AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                  AT.Op sampleLoc AT.Add (AT.Var sampleLoc "myFloat" AT.TFloat) (AT.Var sampleLoc "myInt" (AT.TInt 32))
                ]
      let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedOperator AT.Add)
      testError expectedError funcExpr

    H.describe "generateUnaryOp" $ do
      H.it "should generate unary operator (PreInc)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.PreInc (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.PreInc (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.PreInc (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.PreInc (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

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

        case drop 2 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a third Load instruction"

        case drop 3 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.FloatingPointType T.FloatFP, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a fourth Load instruction"

        case drop 4 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.FloatingPointType T.DoubleFP, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a fifth Load instruction"

        case drop 5 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a sixth Load instruction"

        case drop 6 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a seventh Load instruction"

        case drop 7 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a eighth Load instruction"

        case drop 8 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.FloatingPointType T.FloatFP, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a nineth Load instruction"

        case drop 9 (filter isLoadInstr instrs) of
          (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
            TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.FloatingPointType T.DoubleFP, T.pointerAddrSpace = AS.AddrSpace 0}
            v `H.shouldBe` False
            al `H.shouldBe` 0
          _ -> H.expectationFailure "Expected a tenth Load instruction"

      H.it "should generate unary operator (PreDec)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.PreDec (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.PreDec (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.PreDec (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.PreDec (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should generate unary operator (PostDec)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.PostDec (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.PostDec (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.PostDec (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.PostDec (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should generate unary operator (Not)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.Not (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.Not (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.Not (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.Not (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should generate unary operator (BitNot)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.BitNot (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.BitNot (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.BitNot (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should generate unary operator (Deref)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.Deref (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.Deref (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.Deref (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.Deref (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should generate unary operator (AddrOf)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.AddrOf (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.AddrOf (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.AddrOf (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.AddrOf (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should generate unary operator (PostInc)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "myPtr" (AT.TPointer (AT.TInt 32)) (Just (AT.Var sampleLoc "myInt" (AT.TInt 32))),
                    AT.Declaration sampleLoc "myFloat" AT.TFloat (Just (AT.Lit sampleLoc (AT.LFloat 0.0))),
                    AT.Declaration sampleLoc "myDouble" AT.TDouble (Just (AT.Lit sampleLoc (AT.LDouble 0.0))),
                    AT.UnaryOp sampleLoc AT.PostInc (AT.Var sampleLoc "myInt" (AT.TInt 32)),
                    AT.UnaryOp sampleLoc AT.PostInc (AT.Var sampleLoc "myPtr" (AT.TPointer (AT.TInt 32))),
                    AT.UnaryOp sampleLoc AT.PostInc (AT.Var sampleLoc "myFloat" AT.TFloat),
                    AT.UnaryOp sampleLoc AT.PostInc (AT.Var sampleLoc "myDouble" AT.TDouble)
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        instrs `H.shouldBe` instrs
        length blocks `H.shouldBe` 1

      H.it "should throw an error for unsupported unary operator type (PreInc)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" AT.TVoid Nothing,
                    AT.UnaryOp sampleLoc AT.PreInc (AT.Var sampleLoc "myInt" AT.TVoid)
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedUnaryOperator AT.PreInc)
        testError expectedError funcExpr

      H.it "should throw an error for unsupported unary operator type (PreDec)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" AT.TVoid Nothing,
                    AT.UnaryOp sampleLoc AT.PreDec (AT.Var sampleLoc "myInt" AT.TVoid)
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedUnaryOperator AT.PreDec)
        testError expectedError funcExpr

      H.it "should throw an error for unsupported unary operator type (PostDec)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" AT.TVoid Nothing,
                    AT.UnaryOp sampleLoc AT.PostDec (AT.Var sampleLoc "myInt" AT.TVoid)
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedUnaryOperator AT.PostDec)
        testError expectedError funcExpr

      H.it "should throw an error for unsupported unary operator type (Not)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" AT.TVoid Nothing,
                    AT.UnaryOp sampleLoc AT.Not (AT.Var sampleLoc "myInt" AT.TVoid)
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedUnaryOperator AT.Not)
        testError expectedError funcExpr

      H.it "should throw an error for unsupported unary operator type (BitNot)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" AT.TVoid Nothing,
                    AT.UnaryOp sampleLoc AT.BitNot (AT.Var sampleLoc "myInt" AT.TVoid)
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedUnaryOperator AT.BitNot)
        testError expectedError funcExpr

      H.it "should throw an error for unsupported unary operator type (PostInc)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "myInt" AT.TVoid Nothing,
                    AT.UnaryOp sampleLoc AT.PostInc (AT.Var sampleLoc "myInt" AT.TVoid)
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedUnaryOperator AT.PostInc)
        testError expectedError funcExpr
  where
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

    isBitShrInstr (AST.UnName _ AST.:= AST.AShr {}) = True
    isBitShrInstr _ = False

    isAndInstr (AST.UnName _ AST.:= AST.And {}) = True
    isAndInstr _ = False

    isOrInstr (AST.UnName _ AST.:= AST.Or {}) = True
    isOrInstr _ = False

    isICmpInstr (AST.UnName _ AST.:= AST.ICmp {}) = True
    isICmpInstr _ = False

    isFAddInstr (AST.UnName _ AST.:= AST.FAdd {}) = True
    isFAddInstr _ = False

    isFSubInstr (AST.UnName _ AST.:= AST.FSub {}) = True
    isFSubInstr _ = False

    isFMulInstr (AST.UnName _ AST.:= AST.FMul {}) = True
    isFMulInstr _ = False

    isFDivInstr (AST.UnName _ AST.:= AST.FDiv {}) = True
    isFDivInstr _ = False

    isFmcpInstr (AST.UnName _ AST.:= AST.FCmp {}) = True
    isFmcpInstr _ = False

    isFModInstr (AST.UnName _ AST.:= AST.FRem {}) = True
    isFModInstr _ = False
