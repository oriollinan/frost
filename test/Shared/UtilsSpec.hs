module Shared.UtilsSpec where

import qualified Ast.Types as AT
import Shared.Utils (getLoc)
import Test.Hspec

dummyLoc :: AT.SrcLoc
dummyLoc = AT.SrcLoc {AT.srcFile = "test.file", AT.srcLine = 1, AT.srcCol = 1}

dummyLiteral :: AT.Literal
dummyLiteral = AT.LInt 42

dummyType :: AT.Type
dummyType = AT.TInt 32

dummyExpr :: AT.Expr
dummyExpr = AT.Lit dummyLoc dummyLiteral

spec :: Spec
spec = do
  describe "getLoc" $ do
    it "returns the correct location for Lit" $ do
      let expr = AT.Lit dummyLoc dummyLiteral
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Var" $ do
      let expr = AT.Var dummyLoc "x" dummyType
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for StructAccess" $ do
      let expr = AT.StructAccess dummyLoc dummyExpr dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for ArrayAccess" $ do
      let expr = AT.ArrayAccess dummyLoc dummyExpr dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for UnaryOp" $ do
      let expr = AT.UnaryOp dummyLoc AT.Not dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Call" $ do
      let expr = AT.Call dummyLoc dummyExpr [dummyExpr]
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for If" $ do
      let expr = AT.If dummyLoc dummyExpr dummyExpr (Just dummyExpr)
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for While" $ do
      let expr = AT.While dummyLoc dummyExpr dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for From" $ do
      let expr = AT.From dummyLoc dummyExpr dummyExpr dummyExpr dummyExpr dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Return" $ do
      let expr = AT.Return dummyLoc (Just dummyExpr)
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Break" $ do
      let expr = AT.Break dummyLoc
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Continue" $ do
      let expr = AT.Continue dummyLoc
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Cast" $ do
      let expr = AT.Cast dummyLoc dummyType dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Declaration" $ do
      let expr = AT.Declaration dummyLoc "x" dummyType (Just dummyExpr)
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Assignment" $ do
      let expr = AT.Assignment dummyLoc dummyExpr dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Op" $ do
      let expr = AT.Op dummyLoc AT.Add dummyExpr dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Function" $ do
      let expr = AT.Function dummyLoc "func" dummyType ["param1"] dummyExpr
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for ForeignFunction" $ do
      let expr = AT.ForeignFunction dummyLoc "foreignFunc" dummyType
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Block" $ do
      let exprs = [AT.Lit dummyLoc dummyLiteral, AT.Lit (dummyLoc {AT.srcLine = 2}) dummyLiteral]
      let expr = AT.Block exprs
      getLoc expr `shouldBe` dummyLoc

    it "returns the correct location for Assembly" $ do
      let asmConstraint = AT.AsmConstraint "output" ["input1", "input2"]
      let asmExpr =
            AT.AsmExpr
              { AT.asmCode = "mov eax, ebx",
                AT.asmConstraints = asmConstraint,
                AT.asmArgs = [dummyExpr],
                AT.asmParameters = [dummyType],
                AT.asmReturnType = dummyType,
                AT.asmSideEffects = False,
                AT.asmAlignStack = False,
                AT.asmDialect = AT.Intel
              }
      let expr = AT.Assembly dummyLoc asmExpr
      getLoc expr `shouldBe` dummyLoc
