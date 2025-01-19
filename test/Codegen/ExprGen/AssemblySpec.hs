{-# LANGUAGE OverloadedStrings #-}

module Codegen.ExprGen.AssemblySpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.CallingConvention as ACC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.InlineAssembly as IA
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Assembly" $ do
  H.describe "generateAssembly" $ do
    H.it "generates Intel dialect assembly correctly" $ do
      let expr = makeAsmExpr "mov eax, ebx" AT.Intel AT.TVoid [] [] "" False False
      verifyAssembly expr $ \asm -> do
        IA.dialect asm `H.shouldBe` IA.IntelDialect
        IA.assembly asm `H.shouldBe` "mov eax, ebx"
        IA.hasSideEffects asm `H.shouldBe` False
        IA.alignStack asm `H.shouldBe` False

    H.it "generates AT&T dialect with operands correctly" $ do
      let expr =
            makeAsmExpr
              "mov $1, $0\nadd $0, $0, $2"
              AT.ATT
              (AT.TInt 32)
              [AT.TInt 32, AT.TInt 32]
              [makeLiteral 1, makeLiteral 1]
              "r,r"
              False
              False
      verifyAssembly expr $ \asm -> do
        IA.type' asm `H.shouldBe` AST.FunctionType (AST.IntegerType 32) [AST.IntegerType 32, AST.IntegerType 32] False
        IA.dialect asm `H.shouldBe` IA.ATTDialect
        IA.constraints asm `H.shouldBe` "=r,r,r"

    H.it "handles side effects and stack alignment" $ do
      let expr =
            makeAsmExpr
              "movl %ebx, %eax"
              AT.ATT
              (AT.TInt 32)
              [AT.TInt 32]
              [makeLiteral 0]
              "r"
              True
              True
      verifyAssembly expr $ \asm -> do
        IA.hasSideEffects asm `H.shouldBe` True
        IA.alignStack asm `H.shouldBe` True

    H.it "should generate call instruction" $ do
      let expr =
            makeAsmExpr
              "movl %ebx, %eax"
              AT.ATT
              (AT.TInt 32)
              [AT.TInt 32]
              [makeLiteral 0]
              "r"
              True
              True

      let blocks = generateBlocks expr
      let instrs = concatMap blockInstructions blocks

      case L.find isCallInstr instrs of
        Just (AST.UnName _ AST.:= AST.Call {AST.tailCallKind = tc, AST.callingConvention = c, AST.returnAttributes = r, AST.function = f, AST.arguments = a, AST.functionAttributes = fa, AST.metadata = m}) -> do
          tc `H.shouldBe` Nothing
          c `H.shouldBe` ACC.C
          r `H.shouldBe` []
          case f of
            Left _ -> pure ()
            Right _ -> H.expectationFailure "Expected a left function"
          a `H.shouldBe` [(AST.ConstantOperand (C.Int 32 0), [])]
          fa `H.shouldBe` []
          m `H.shouldBe` []
        _ -> H.expectationFailure "Expected a call instruction"
  where
    sampleLoc = AT.SrcLoc "test.c" 1 1

    makeAsmExpr code dialect retType params args constraints sideEffects alignStack =
      AT.Assembly sampleLoc $
        AT.AsmExpr
          { AT.asmCode = code,
            AT.asmDialect = dialect,
            AT.asmReturnType = retType,
            AT.asmParameters = params,
            AT.asmArgs = args,
            AT.asmConstraints = AT.AsmConstraint "r" [constraints],
            AT.asmSideEffects = sideEffects,
            AT.asmAlignStack = alignStack
          }

    makeLiteral n = AT.Lit sampleLoc (AT.LInt n)

    verifyAssembly expr check = do
      let blocks = generateBlocks expr
      let instrs = concatMap blockInstructions blocks
      case findAsmInstruction instrs of
        Just asm -> check asm
        Nothing -> H.expectationFailure "No assembly instruction found"

    generateBlocks expr =
      case CC.codegen (AT.Program [("test", wrapInFunction expr)] [] "test.c") of
        Right mod' -> concatMap G.basicBlocks [f | AST.GlobalDefinition f@AST.Function {} <- AST.moduleDefinitions mod']
        Left _ -> []

    wrapInFunction expr =
      AT.Function
        { AT.funcLoc = sampleLoc,
          AT.funcName = "test",
          AT.funcType = AT.TFunction AT.TVoid [] False,
          AT.funcParams = [],
          AT.funcBody = expr
        }

    blockInstructions (G.BasicBlock _ instrs _) = instrs

    findAsmInstruction instrs =
      L.find isAsmInstruction instrs >>= extractAsm

    isAsmInstruction (AST.Do (AST.Call {AST.function = Left _})) = True
    isAsmInstruction (AST.UnName _ AST.:= AST.Call {AST.function = Left _}) = True
    isAsmInstruction _ = False

    extractAsm (AST.Do (AST.Call {AST.function = Left asm})) = Just asm
    extractAsm (AST.UnName _ AST.:= AST.Call {AST.function = Left asm}) = Just asm
    extractAsm _ = Nothing

    isCallInstr (AST.UnName _ AST.:= AST.Call {}) = True
    isCallInstr _ = False
