{-# LANGUAGE OverloadedStrings #-}

module Codegen.ErrorsSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CE
import qualified LLVM.AST.Type as T
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "Codegen.Errors" $ do
  H.describe "CodegenError" $ do
    H.it "should create a CodegenError with correct location and type" $ do
      let loc = AT.SrcLoc "test.c" 1 1
      let err = CE.CodegenError loc (CE.VariableNotFound "x")
      CE.errorLoc err `H.shouldBe` loc
      CE.errorType err `H.shouldBe` CE.VariableNotFound "x"

  H.describe "Show instance for CodegenError" $ do
    H.it "should format error message correctly" $ do
      let loc = AT.SrcLoc "test.c" 1 1
      let err = CE.CodegenError loc (CE.VariableNotFound "x")
      show err `H.shouldBe` "test.c:1:1: Variable not found: x"

  H.describe "showErrorType" $ do
    H.it "should format UnsupportedTopLevel correctly" $ do
      let expr = AT.Lit (AT.SrcLoc "test.c" 1 1) (AT.LInt 42)
      CE.showErrorType (CE.UnsupportedTopLevel expr) `H.shouldBe` "Unsupported top-level expression: Lit (SrcLoc {srcFile = \"test.c\", srcLine = 1, srcCol = 1}) (LInt 42)"

    H.it "should format UnsupportedOperator correctly" $ do
      CE.showErrorType (CE.UnsupportedOperator AT.Add) `H.shouldBe` "Unsupported operator: Add"

    H.it "should format UnsupportedUnaryOperator correctly" $ do
      CE.showErrorType (CE.UnsupportedUnaryOperator AT.Not) `H.shouldBe` "Unsupported unary operator: Not"

    H.it "should format UnsupportedLiteral correctly" $ do
      CE.showErrorType (CE.UnsupportedLiteral (AT.LInt 42)) `H.shouldBe` "Unsupported literal: LInt 42"

    H.it "should format UnsupportedType correctly" $ do
      CE.showErrorType (CE.UnsupportedType AT.TVoid) `H.shouldBe` "Unsupported type: TVoid"

    H.it "should format VariableNotFound correctly" $ do
      CE.showErrorType (CE.VariableNotFound "x") `H.shouldBe` "Variable not found: x"

    H.it "should format UnsupportedFunctionCall correctly" $ do
      CE.showErrorType (CE.UnsupportedFunctionCall "foo") `H.shouldBe` "Invalid function call: foo"

    H.it "should format ContinueOutsideLoop correctly" $ do
      CE.showErrorType CE.ContinueOutsideLoop `H.shouldBe` "Continue statement outside loop"

    H.it "should format BreakOutsideLoop correctly" $ do
      CE.showErrorType CE.BreakOutsideLoop `H.shouldBe` "Break statement outside loop"

    H.it "should format UnsupportedConversion correctly" $ do
      CE.showErrorType (CE.UnsupportedConversion T.i32 T.float) `H.shouldBe` "Unsupported conversion from IntegerType {typeBits = 32} to FloatingPointType {floatingPointType = FloatFP}"

    H.it "should format UnsupportedTopLevel" $ do
      let expr = AT.Lit (AT.SrcLoc "test.c" 1 1) (AT.LInt 42)
      CE.showErrorType (CE.UnsupportedTopLevel expr)
        `H.shouldBe` "Unsupported top-level expression: Lit (SrcLoc {srcFile = \"test.c\", srcLine = 1, srcCol = 1}) (LInt 42)"

    H.it "should format loop control errors" $ do
      CE.showErrorType CE.ContinueOutsideLoop `H.shouldBe` "Continue statement outside loop"
      CE.showErrorType CE.BreakOutsideLoop `H.shouldBe` "Break statement outside loop"

    H.it "should format structure errors" $ do
      CE.showErrorType (CE.StructureFieldNotFound "field1")
        `H.shouldBe` "Structure field not found: field1"

    H.it "should format function errors" $ do
      CE.showErrorType (CE.UnsupportedFunctionCall "main")
        `H.shouldBe` "Invalid function call: main"

    H.it "should format literal errors" $ do
      CE.showErrorType (CE.UnsupportedLiteral (AT.LArray []))
        `H.shouldBe` "Unsupported literal: LArray []"

    H.it "should format loop definition errors" $ do
      let expr = AT.Lit (AT.SrcLoc "test.c" 1 1) (AT.LInt 0)
      CE.showErrorType (CE.UnsupportedForDefinition expr)
        `H.shouldBe` "Invalid for loop: Lit (SrcLoc {srcFile = \"test.c\", srcLine = 1, srcCol = 1}) (LInt 0)"
      CE.showErrorType (CE.UnsupportedWhileDefinition expr)
        `H.shouldBe` "Invalid while loop: Lit (SrcLoc {srcFile = \"test.c\", srcLine = 1, srcCol = 1}) (LInt 0)"

    H.it "should format global declaration errors" $ do
      let expr = AT.Lit (AT.SrcLoc "test.c" 1 1) (AT.LInt 42)
      CE.showErrorType (CE.UnsupportedGlobalDeclaration expr)
        `H.shouldBe` "Unsupported global declaration: Lit (SrcLoc {srcFile = \"test.c\", srcLine = 1, srcCol = 1}) (LInt 42)"

    H.it "should format variable errors" $ do
      let lit = AT.LInt 42
      CE.showErrorType (CE.UnsupportedGlobalVar lit) `H.shouldBe` "Unsupported global variable: LInt 42"
      CE.showErrorType (CE.UnsupportedLocalVar lit) `H.shouldBe` "Unsupported local variable: LInt 42"
