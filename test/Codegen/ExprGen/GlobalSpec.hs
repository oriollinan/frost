module Codegen.ExprGen.GlobalSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Codegen.Errors as CE
import qualified Codegen.Utils as U
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.Global" $ do
  H.describe "generateGlobal" $ do
    H.it "should generate global function" $ do
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

    H.it "should generate global foreign function" $ do
      let funcExpr =
            AT.ForeignFunction
              { AT.funcLoc = sampleLoc,
                AT.funcName = "test",
                AT.funcType = AT.TFunction AT.TVoid [] True
              }
      let blocks = generateTestBlocks funcExpr
      length blocks `H.shouldBe` 0

  H.it "should throw a CodegenError for unsupported global expressions" $ do
    let invalidExpr = AT.Lit sampleLoc (AT.LInt 0)
    let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedTopLevel invalidExpr)
    testError expectedError invalidExpr

  H.describe "generateGlobalDeclaration" $ do
    H.it "should generate global declaration" $ do
      let funcExpr =
            AT.Declaration
              { AT.declLoc = sampleLoc,
                AT.declName = "test",
                AT.declType = AT.TInt 32,
                AT.declInit = Just (AT.Lit sampleLoc (AT.LInt 0))
              }
      let globals = generateGlobalDeclaration funcExpr
      G.name (head globals) `H.shouldBe` AST.Name (U.stringToByteString "test")
      G.type' (head globals) `H.shouldBe` T.IntegerType 32
      G.initializer (head globals) `H.shouldBe` Just (C.Int 32 0)
      length globals `H.shouldBe` 1

    H.it "should generate global declaration without initialization" $ do
      let funcExpr =
            AT.Declaration
              { AT.declLoc = sampleLoc,
                AT.declName = "test",
                AT.declType = AT.TInt 32,
                AT.declInit = Nothing
              }
      let globals = generateGlobalDeclaration funcExpr
      G.name (head globals) `H.shouldBe` AST.Name (U.stringToByteString "test")
      G.type' (head globals) `H.shouldBe` T.IntegerType 32
      G.initializer (head globals) `H.shouldBe` Just (C.Undef (T.IntegerType 32))
      length globals `H.shouldBe` 1

    H.it "should throw an error if initialization does not match Nothing or a Literal" $ do
      let invalidInit = AT.Var sampleLoc "test" (AT.TInt 32)
      let declarationExpr =
            AT.Declaration
              { AT.declLoc = sampleLoc,
                AT.declName = "test",
                AT.declType = AT.TInt 32,
                AT.declInit = Just invalidInit
              }
      let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedGlobalDeclaration invalidInit)
      testError expectedError declarationExpr
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

    generateGlobalDeclaration expr = case CC.codegen testProg of
      Right mod' -> getGlobalVariables mod'
      Left _ -> []
      where
        testProg = AT.Program [("test", expr)] [] "test.c"

    getDefinitions mod' =
      [f | AST.GlobalDefinition f@(AST.Function {}) <- AST.moduleDefinitions mod']

    getGlobalVariables mod' =
      [g | AST.GlobalDefinition g@(AST.GlobalVariable {}) <- AST.moduleDefinitions mod']
