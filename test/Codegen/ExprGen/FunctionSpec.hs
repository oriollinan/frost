module Codegen.ExprGen.FunctionSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as C
import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "Codegen" $ do
  H.describe "generateFunction" $ do
    H.it "should generate function without parametes" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction (AT.TInt 32) [] True,
                AT.funcParams = [],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate function with void return type" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction AT.TVoid [] True,
                AT.funcParams = [],
                AT.funcBody =
                  AT.Block
                    []
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate function with parameters" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction (AT.TInt 32) [AT.TInt 32] False,
                AT.funcParams = ["x"],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1

    H.it "should generate function with function in its parameters" $ do
      let funcExpr =
            AT.Function
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction (AT.TInt 32) [AT.TBoolean, AT.TFunction (AT.TInt 32) [AT.TInt 32] False, AT.TFunction (AT.TInt 32) [AT.TInt 32] False] False,
                AT.funcParams = ["bool", "x", "y"],
                AT.funcBody =
                  AT.Block
                    [ AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                    ]
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 1
  where
    sampleLoc = AT.SrcLoc "test.c" 1 1

    generateTestBlocks expr = case C.codegen testProg of
      Right mod' -> concatMap G.basicBlocks $ getDefinitions mod'
      Left _ -> []
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']
