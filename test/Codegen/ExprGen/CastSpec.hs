module Codegen.ExprGen.CastSpec (spec) where

-- Add this import at the top of your file
import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Codegen.Errors as CE
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as FF
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "Codegen" $ do
  H.context "when testing individual codegen functions" $ do
    let wrapInFunction expr =
          AT.Function
            { AT.funcLoc = sampleLoc,
              AT.funcName = "test",
              AT.funcType = AT.TFunction AT.TVoid [] False,
              AT.funcParams = [],
              AT.funcBody =
                expr
            }
    H.describe "generateCast" $ do
      H.it "should generate a cast (Integer -> Integer)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Cast sampleLoc (AT.TInt 64) (AT.Lit sampleLoc (AT.LInt 0)),
                    AT.Cast sampleLoc (AT.TInt 8) (AT.Lit sampleLoc (AT.LInt 0)),
                    AT.Cast sampleLoc (AT.TInt 32) (AT.Lit sampleLoc (AT.LInt 0))
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        length blocks `H.shouldBe` 1
        case L.find isZextInstr instrs of
          Just (AST.UnName _ AST.:= AST.ZExt {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` T.IntegerType 64
            o `H.shouldBe` AST.ConstantOperand (C.Int 32 0)
          _ -> H.expectationFailure "Expected a Zext instruction"

        case L.find isTruncInstr instrs of
          Just (AST.UnName _ AST.:= AST.Trunc {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` T.IntegerType 8
            o `H.shouldBe` AST.ConstantOperand (C.Int 32 0)
          _ -> H.expectationFailure "Expected a Trunc instruction"

      H.it "should generate a cast (Float -> Float)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Cast sampleLoc AT.TDouble (AT.Lit sampleLoc (AT.LFloat 0)),
                    AT.Cast sampleLoc AT.TFloat (AT.Lit sampleLoc (AT.LDouble 0)),
                    AT.Cast sampleLoc AT.TFloat (AT.Lit sampleLoc (AT.LFloat 0))
                  ]

        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        length blocks `H.shouldBe` 1
        case L.find isFPExtInstr instrs of
          Just (AST.UnName _ AST.:= AST.FPExt {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` AST.FloatingPointType T.DoubleFP
            o `H.shouldBe` AST.ConstantOperand (C.Float (FF.Single 0))
          _ -> H.expectationFailure "Expected a FPExt instruction"

        case L.find isFPTruncInstr instrs of
          Just (AST.UnName _ AST.:= AST.FPTrunc {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` AST.FloatingPointType T.FloatFP
            o `H.shouldBe` AST.ConstantOperand (C.Float (FF.Double 0))
          _ -> H.expectationFailure "Expected a FPTrunc instruction"

      H.it "should generate a cast (Integer <-> Float)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Cast sampleLoc AT.TFloat (AT.Lit sampleLoc (AT.LInt 0)),
                    AT.Cast sampleLoc (AT.TInt 32) (AT.Lit sampleLoc (AT.LFloat 0))
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        length blocks `H.shouldBe` 1
        case L.find isSIToFPInstr instrs of
          Just (AST.UnName _ AST.:= AST.SIToFP {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` AST.FloatingPointType T.FloatFP
            o `H.shouldBe` AST.ConstantOperand (C.Int 32 0)
          _ -> H.expectationFailure "Expected a SIToFP instruction"

        case L.find isFPToSIInstr instrs of
          Just (AST.UnName _ AST.:= AST.FPToSI {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` T.IntegerType 32
            o `H.shouldBe` AST.ConstantOperand (C.Float (FF.Single 0))
          _ -> H.expectationFailure "Expected a FPToSI instruction"

      H.it "should generate a cast (Bitcast)" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "ptrToInt" (AT.TPointer (AT.TInt 32)) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Declaration sampleLoc "int" (AT.TInt 32) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Cast sampleLoc (AT.TPointer AT.TFloat) (AT.Var sampleLoc "ptrToInt" (AT.TPointer (AT.TInt 32))),
                    AT.Cast sampleLoc (AT.TPointer (AT.TInt 32)) (AT.Var sampleLoc "int" (AT.TInt 32)),
                    AT.Cast sampleLoc (AT.TInt 32) (AT.Var sampleLoc "ptrToInt" (AT.TPointer (AT.TInt 32)))
                  ]
        let blocks = generateTestBlocks funcExpr
        let instrs = getInstructions blocks

        length blocks `H.shouldBe` 1
        case L.find isBitCastInstr instrs of
          Just (AST.UnName _ AST.:= AST.BitCast {AST.operand0 = o, AST.type' = t}) -> do
            t `H.shouldBe` AST.PointerType (AST.IntegerType 32) (AS.AddrSpace 0)
            o `H.shouldBe` AST.ConstantOperand (C.Int 32 0)
          _ -> H.expectationFailure "Expected a BitCast instruction"

        case drop 1 (filter isBitCastInstr instrs) of
          (AST.UnName _ AST.:= AST.BitCast {AST.operand0 = o, AST.type' = t} : _) -> do
            t `H.shouldBe` AST.PointerType (AST.FloatingPointType T.FloatFP) (AS.AddrSpace 0)
            TD.typeOf o `H.shouldBe` AST.PointerType (AST.IntegerType 32) (AS.AddrSpace 0)
          _ -> H.expectationFailure "Expected a second BitCast instruction"

        case drop 2 (filter isBitCastInstr instrs) of
          (AST.UnName _ AST.:= AST.BitCast {AST.operand0 = o, AST.type' = t} : _) -> do
            t `H.shouldBe` AST.PointerType (AST.IntegerType 32) (AS.AddrSpace 0)
            TD.typeOf o `H.shouldBe` AST.IntegerType 32
          _ -> H.expectationFailure "Expected a third BitCast instruction"

      H.it "should throw an error if a cast is not possible" $ do
        let funcExpr =
              wrapInFunction $
                AT.Block
                  [ AT.Declaration sampleLoc "int" (AT.TInt 1) (Just (AT.Lit sampleLoc (AT.LInt 0))),
                    AT.Cast sampleLoc AT.TVoid (AT.Var sampleLoc "int" (AT.TInt 1))
                  ]
        let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedConversion (T.IntegerType 1) T.void)
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
      Left _ -> []
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']

    getInstructions blocks =
      [i | G.BasicBlock _ instrs _ <- blocks, i <- instrs]

    isZextInstr (AST.UnName _ AST.:= AST.ZExt {}) = True
    isZextInstr _ = False

    isTruncInstr (AST.UnName _ AST.:= AST.Trunc {}) = True
    isTruncInstr _ = False

    isFPExtInstr (AST.UnName _ AST.:= AST.FPExt {}) = True
    isFPExtInstr _ = False

    isFPTruncInstr (AST.UnName _ AST.:= AST.FPTrunc {}) = True
    isFPTruncInstr _ = False

    isSIToFPInstr (AST.UnName _ AST.:= AST.SIToFP {}) = True
    isSIToFPInstr _ = False

    isFPToSIInstr (AST.UnName _ AST.:= AST.FPToSI {}) = True
    isFPToSIInstr _ = False

    isBitCastInstr (AST.UnName _ AST.:= AST.BitCast {}) = True
    isBitCastInstr _ = False
