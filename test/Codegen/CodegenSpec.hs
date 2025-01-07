module Codegen.CodegenSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as C
import qualified Codegen.Utils as U
import qualified LLVM.AST as AST
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Type as T
import qualified Test.Hspec as H

{--#
Simple program which adds two numbers and
returns 1 if the result is greater than 30,
otherwise 0.

It's equivalent to the following C code:

```
int main(void)
{
    int x = 10;
    int y = 20;
    int result = x + y;

    if (result > 30) {
        return 1;
    } else {
        return 0;
    }
}
```
#--}
simpleSum :: AT.Program
simpleSum =
  AT.Program
    { AT.globals = [("main", mainDef)],
      AT.types = [],
      AT.sourceFile = "sample.c"
    }
  where
    sampleLoc = AT.SrcLoc "sample.c" 1 1
    mainDef =
      AT.Function
        { AT.funcLoc = sampleLoc,
          AT.funcName = "main",
          AT.funcType = AT.TFunction (AT.TInt 32) [AT.TInt 32] False,
          AT.funcParams = [],
          AT.funcBody =
            AT.Block
              [ AT.Declaration
                  sampleLoc
                  "x"
                  (AT.TInt 32)
                  (Just (AT.Lit sampleLoc (AT.LInt 10))),
                AT.Declaration
                  sampleLoc
                  "y"
                  (AT.TInt 32)
                  (Just (AT.Lit sampleLoc (AT.LInt 20))),
                AT.Declaration
                  sampleLoc
                  "result"
                  (AT.TInt 32)
                  ( Just
                      ( AT.Op
                          sampleLoc
                          AT.Add
                          (AT.Var sampleLoc "x" (AT.TInt 32))
                          (AT.Var sampleLoc "y" (AT.TInt 32))
                      )
                  ),
                AT.If
                  sampleLoc
                  ( AT.Op
                      sampleLoc
                      AT.Gt
                      (AT.Var sampleLoc "result" (AT.TInt 32))
                      (AT.Lit sampleLoc (AT.LInt 30))
                  )
                  ( AT.Return
                      sampleLoc
                      (Just (AT.Lit sampleLoc (AT.LInt 1)))
                  )
                  ( Just
                      ( AT.Return
                          sampleLoc
                          (Just (AT.Lit sampleLoc (AT.LInt 0)))
                      )
                  )
              ]
        }

spec :: H.Spec
spec = H.describe "Codegen" $ do
  H.context "when generating code for a simple sum program" $ do
    let getMainFunc mod' = do
          let defs = AST.moduleDefinitions mod'
          let mainFuncs =
                [ f
                  | AST.GlobalDefinition f@(AST.Function {}) <- defs,
                    G.name f == AST.Name (U.stringToByteString "main")
                ]
          head mainFuncs

    let withModule test = do
          case C.codegen simpleSum of
            Left err ->
              H.expectationFailure $ "Failed to generate module: " ++ show err
            Right mod' -> test mod'

    H.it "should generate a valid module with main function" $ withModule $ \mod' -> do
      let mainFunc = getMainFunc mod'
      let (mainParams, isVarArg) = G.parameters mainFunc
      length mainParams `H.shouldBe` 0
      isVarArg `H.shouldBe` False

    H.it "should have correct function type" $ withModule $ \mod' -> do
      let mainFunc = getMainFunc mod'
      G.returnType mainFunc `H.shouldBe` T.i32

    H.it "should have the correct number of basic blocks" $ withModule $ \mod' -> do
      let mainFunc = getMainFunc mod'
      length (G.basicBlocks mainFunc) `H.shouldBe` 4

    H.it "should have correct module name" $ withModule $ \mod' ->
      AST.moduleName mod' `H.shouldBe` U.stringToByteString "sample.c"

    H.it "should not have internal, aka static, functions" $ withModule $ \mod' -> do
      let internals =
            [ f
              | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod',
                G.linkage f == L.Internal
            ]
      length internals `H.shouldBe` 0

    H.it "should have external linkage for main" $ withModule $ \mod' -> do
      let mainFunc = getMainFunc mod'
      G.linkage mainFunc `H.shouldBe` L.External

    H.it "should have conditional branch instruction" $ withModule $ \mod' -> do
      let mainFunc = getMainFunc mod'
      let blocks = G.basicBlocks mainFunc
      let (G.BasicBlock _ _ terminator) = head blocks
      case terminator of
        AST.Do (AST.CondBr {}) -> return ()
        _ -> H.expectationFailure "Expected conditional branch instruction"

  H.context "when testing individual codegen functions" $ do
    let wrapInFunction expr =
          AT.Function
            { AT.funcLoc = sampleLoc,
              AT.funcName = "test",
              AT.funcType = AT.TFunction (AT.TInt 32) [] False,
              AT.funcParams = [],
              AT.funcBody =
                AT.Block
                  [ expr,
                    AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))
                  ]
            }

    H.describe "generateIf" $ do
      H.it "should generate correct branch structure" $ do
        let ifExpr =
              AT.If
                sampleLoc
                (AT.Lit sampleLoc (AT.LBool True))
                (AT.Lit sampleLoc (AT.LInt 1))
                (Just (AT.Lit sampleLoc (AT.LInt 0)))

        let blocks = generateTestBlocks (wrapInFunction ifExpr)
        length blocks `H.shouldBe` 4

    H.describe "generateVar" $ do
      H.it "should handle variable lookup correctly" $ do
        let varExpr = AT.Var sampleLoc "x" (AT.TInt 32)
        let varDecl =
              AT.Declaration
                sampleLoc
                "x"
                (AT.TInt 32)
                (Just (AT.Lit sampleLoc (AT.LInt 42)))

        let blocks = generateTestBlocks (wrapInFunction (AT.Block [varDecl, varExpr]))
        length blocks `H.shouldBe` 1

    H.describe "generateBinaryOp" $ do
      H.it "should generate correct arithmetic operations" $ do
        let addExpr =
              AT.Op
                sampleLoc
                AT.Add
                (AT.Lit sampleLoc (AT.LInt 5))
                (AT.Lit sampleLoc (AT.LInt 3))

        let blocks = generateTestBlocks (wrapInFunction addExpr)
        let instrs = getInstructions blocks
        any isAddInstr instrs `H.shouldBe` True

    H.describe "generateFunction" $ do
      H.it "should create function with correct signature" $ do
        let funcExpr =
              AT.Function
                sampleLoc
                "test"
                (AT.TFunction (AT.TInt 32) [] False)
                []
                (AT.Block [AT.Return sampleLoc (Just (AT.Lit sampleLoc (AT.LInt 0)))])

        let blocks = generateTestBlocks funcExpr
        length blocks `H.shouldBe` 1

    H.describe "generateDeclaration" $ do
      H.it "should allocate and initialize variables" $ do
        let declExpr =
              AT.Declaration
                sampleLoc
                "x"
                (AT.TInt 32)
                (Just (AT.Lit sampleLoc (AT.LInt 42)))

        let blocks = generateTestBlocks (wrapInFunction declExpr)
        let instrs = getInstructions blocks
        any isAllocaInstr instrs `H.shouldBe` True
        any isStoreInstr instrs `H.shouldBe` True
  where
    sampleLoc = AT.SrcLoc "test.c" 1 1

    generateTestBlocks expr = case C.codegen testProg of
      Right mod' -> concatMap G.basicBlocks $ getDefinitions mod'
      Left _ -> []
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']

    getInstructions blocks =
      [i | G.BasicBlock _ instrs _ <- blocks, i <- instrs]

    isAddInstr (AST.UnName _ AST.:= AST.Add {}) = True
    isAddInstr _ = False

    isAllocaInstr (AST.UnName _ AST.:= AST.Alloca {}) = True
    isAllocaInstr _ = False

    isStoreInstr (AST.Do (AST.Store {})) = True
    isStoreInstr _ = False
