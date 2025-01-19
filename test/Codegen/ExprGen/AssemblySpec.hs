{-# LANGUAGE OverloadedStrings #-}

module Codegen.ExprGen.AssemblySpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.InlineAssembly as IA
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Assembly" $ do
  H.describe "generateAssembly" $ do
    H.it "should generate Intel dialect assembly" $ do
      let funcExpr =
            wrapInFunction $
              AT.Assembly sampleLoc $
                AT.AsmExpr
                  { AT.asmCode = "mov eax, ebx",
                    AT.asmDialect = AT.Intel,
                    AT.asmReturnType = AT.TVoid,
                    AT.asmParameters = [],
                    AT.asmArgs = [],
                    AT.asmConstraints = AT.AsmConstraint "" [],
                    AT.asmSideEffects = False,
                    AT.asmAlignStack = False
                  }

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isInlineAsmInstr instrs of
        Just (AST.Do (AST.Call {AST.function = Left asm})) -> do
          IA.dialect asm `H.shouldBe` IA.IntelDialect
          IA.assembly asm `H.shouldBe` "mov eax, ebx"
        _ -> H.expectationFailure "Expected an inline assembly instruction"

    H.it "should generate AT&T dialect assembly with constraints" $ do
      let funcExpr =
            wrapInFunction $
              AT.Assembly sampleLoc $
                AT.AsmExpr
                  { AT.asmCode = "movl %ebx, %eax",
                    AT.asmDialect = AT.ATT,
                    AT.asmReturnType = AT.TInt 32,
                    AT.asmParameters = [AT.TInt 32],
                    AT.asmArgs = [AT.Lit sampleLoc (AT.LInt 0)],
                    AT.asmConstraints = AT.AsmConstraint "r" ["r"],
                    AT.asmSideEffects = True,
                    AT.asmAlignStack = True
                  }

      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1
      case L.find isInlineAsmInstr instrs of
        Just (AST.UnName _ AST.:= AST.Call {AST.function = Left asm}) -> do
          IA.dialect asm `H.shouldBe` IA.ATTDialect
          IA.hasSideEffects asm `H.shouldBe` True
          IA.alignStack asm `H.shouldBe` True
        _ -> H.expectationFailure "Expected an inline assembly instruction"
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

    isInlineAsmInstr (AST.Do (AST.Call {AST.function = Left _})) = True
    isInlineAsmInstr (AST.UnName _ AST.:= AST.Call {AST.function = Left _}) = True
    isInlineAsmInstr _ = False
