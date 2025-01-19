module Codegen.ExprGen.VariableSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as FF
import qualified LLVM.AST.Global as G
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Variable" $ do
  H.describe "generateDeclaration" $ do
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

    H.it "should handle uninitialized declarations" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "x" (AT.TInt 32) Nothing
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isLoadInstr instrs of
        Just (AST.UnName _ AST.:= AST.Load {}) -> return ()
        _ -> H.expectationFailure "Expected a load instruction"

  H.describe "generateLiteral" $ do
    H.it "should generate integer literal" $ do
      let funcExpr =
            wrapInFunction $
              AT.Op
                sampleLoc
                AT.Add
                ( AT.Lit sampleLoc (AT.LInt 42)
                )
                (AT.Lit sampleLoc (AT.LInt 22))

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isConstant instrs of
        Just (AST.UnName _ AST.:= AST.Add {AST.operand0 = val}) ->
          val `H.shouldBe` AST.ConstantOperand (C.Int 32 42)
        _ -> H.expectationFailure "Expected a constant"

  H.it "should generate float literal" $ do
    let funcExpr =
          wrapInFunction $
            AT.Op
              sampleLoc
              AT.Add
              ( AT.Lit sampleLoc (AT.LFloat 3.14)
              )
              (AT.Lit sampleLoc (AT.LFloat 3.14))

    let blocks = generateTestBlocks funcExpr
    let instrs = getInstructions blocks

    length blocks `H.shouldBe` 1
    case L.find isConstant instrs of
      Just (AST.UnName _ AST.:= AST.FAdd {AST.operand0 = val}) ->
        val `H.shouldBe` AST.ConstantOperand (C.Float (FF.Single 3.14))
      _ -> H.expectationFailure "Expected a constant"

  H.describe "generateAssignment" $ do
    H.it "should generate simple assignment" $ do
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

    H.it "should generate array assignment" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "arr" (AT.TArray (AT.TInt 32) (Just 10)) Nothing,
                  AT.Assignment
                    sampleLoc
                    ( AT.ArrayAccess
                        sampleLoc
                        (AT.Var sampleLoc "arr" (AT.TArray (AT.TInt 32) (Just 10)))
                        (AT.Lit sampleLoc (AT.LInt 0))
                    )
                    (AT.Lit sampleLoc (AT.LInt 42))
                ]

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isGEPInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {}) -> return ()
        _ -> H.expectationFailure "Expected a GEP instruction"
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

    isLoadInstr (AST.UnName _ AST.:= AST.Load {}) = True
    isLoadInstr _ = False

    isConstant (AST.UnName _ AST.:= AST.Add {}) = True
    isConstant (AST.UnName _ AST.:= AST.FAdd {}) = True
    isConstant _ = False

    isGEPInstr (AST.UnName _ AST.:= AST.GetElementPtr {}) = True
    isGEPInstr _ = False
