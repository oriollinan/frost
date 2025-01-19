module Codegen.ExprGen.DataValueSpec (spec) where

import qualified Ast.Types as AT
import qualified Codegen.Codegen as CC
import qualified Codegen.Errors as CE
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified Test.Hspec as H

spec :: H.Spec
spec = H.describe "ExprGen.DataValue" $ do
  let wrapInFunction expr =
        AT.Function
          { AT.funcLoc = sampleLoc,
            AT.funcName = "test",
            AT.funcType = AT.TFunction AT.TVoid [] False,
            AT.funcParams = [],
            AT.funcBody =
              expr
          }
  H.describe "generateArrayAccess" $ do
    H.it "should generate acces to array field" $ do
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "array" (AT.TArray (AT.TInt 32) (Just 10)) (Just (AT.Lit sampleLoc (AT.LArray [AT.LInt 0]))),
                  AT.ArrayAccess sampleLoc (AT.Var sampleLoc "array" (AT.TArray (AT.TInt 32) (Just 10))) (AT.Lit sampleLoc (AT.LInt 0))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1

      case L.find isGepInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) -> do
          TD.typeOf a `H.shouldBe` T.ArrayType 10 (T.IntegerType 32)
          i `H.shouldBe` [AST.ConstantOperand (C.Int 32 0)]
        _ -> H.expectationFailure "Expected a GetElementPtr instruction"

      case L.find isLoadInstr instrs of
        Just (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.ArrayType 10 (T.IntegerType 32), T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
        _ -> H.expectationFailure "Expected a Load instruction"

      case drop 1 (filter isLoadInstr instrs) of
        (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.ArrayType 10 (T.IntegerType 32), T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
          al `H.shouldBe` 0
        _ -> H.expectationFailure "Expected a Load instruction"

      case drop 2 (filter isLoadInstr instrs) of
        (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
          al `H.shouldBe` 0
        _ -> H.expectationFailure "Expected a Load instruction"

  H.describe "generateStructAccess" $ do
    H.it "should generate access to structure field" $ do
      let structType = AT.TStruct "struct" [("field1", AT.TInt 32), ("field2", AT.TInt 32)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "struct" structType (Just (AT.Lit sampleLoc (AT.LStruct [("field1", AT.LInt 0), ("field2", AT.LInt 0)]))),
                  AT.StructAccess sampleLoc (AT.Var sampleLoc "struct" structType) (AT.Var sampleLoc "field1" (AT.TInt 32))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1

      case L.find isGepInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.StructureType False [T.IntegerType 32, T.IntegerType 32], T.pointerAddrSpace = AS.AddrSpace 0}
          i `H.shouldBe` [AST.ConstantOperand (C.Int 32 0), AST.ConstantOperand (C.Int 32 0)]
        _ -> H.expectationFailure "Expected a GetElementPtr instruction"

      case L.find isLoadInstr instrs of
        Just (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.StructureType False [T.IntegerType 32, T.IntegerType 32], T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
        _ -> H.expectationFailure "Expected a second Load instruction"

      case drop 1 (filter isLoadInstr instrs) of
        (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
          al `H.shouldBe` 0
        _ -> H.expectationFailure "Expected a third Load instruction"

  H.describe "generateStructAccess" $ do
    H.it "should generate access to structure field" $ do
      let structType = AT.TStruct "struct" [("field1", AT.TInt 32), ("field2", AT.TInt 32)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "struct" structType (Just (AT.Lit sampleLoc (AT.LStruct [("field1", AT.LInt 0), ("field2", AT.LInt 0)]))),
                  AT.StructAccess sampleLoc (AT.Var sampleLoc "struct" structType) (AT.Var sampleLoc "field1" (AT.TInt 32))
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      length blocks `H.shouldBe` 1

      case L.find isGepInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.StructureType False [T.IntegerType 32, T.IntegerType 32], T.pointerAddrSpace = AS.AddrSpace 0}
          i `H.shouldBe` [AST.ConstantOperand (C.Int 32 0), AST.ConstantOperand (C.Int 32 0)]
        _ -> H.expectationFailure "Expected a GetElementPtr instruction"

      case L.find isLoadInstr instrs of
        Just (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.StructureType False [T.IntegerType 32, T.IntegerType 32], T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
        _ -> H.expectationFailure "Expected a Load instruction"

      case drop 1 (filter isLoadInstr instrs) of
        (AST.UnName _ AST.:= AST.Load {AST.address = a, AST.volatile = v, AST.alignment = al} : _) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.IntegerType 32, T.pointerAddrSpace = AS.AddrSpace 0}
          v `H.shouldBe` False
          al `H.shouldBe` 0
        _ -> H.expectationFailure "Expected a second Load instruction"

    H.it "should generate nested access to structure field" $ do
      let structType = AT.TStruct "example" [("field", AT.TStruct "nestedStruct" [("nestedField", AT.TInt 8)])]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "data" structType Nothing,
                  AT.StructAccess sampleLoc (AT.StructAccess sampleLoc (AT.Var sampleLoc "data" structType) (AT.Var sampleLoc "field" AT.TUnknown)) (AT.Var sampleLoc "nestedField" AT.TUnknown)
                ]
      let blocks = generateTestBlocks funcExpr
      let instrs = getInstructions blocks

      case L.find isGepInstr instrs of
        Just (AST.UnName _ AST.:= AST.GetElementPtr {AST.address = a, AST.indices = i}) -> do
          TD.typeOf a `H.shouldBe` T.PointerType {T.pointerReferent = T.StructureType False [T.StructureType False [T.IntegerType 8]], T.pointerAddrSpace = AS.AddrSpace 0}
          i `H.shouldBe` [AST.ConstantOperand (C.Int 32 0), AST.ConstantOperand (C.Int 32 0)]
        _ -> H.expectationFailure "Expected a GetElementPtr instruction"

    H.it "should throw error if field does not exist" $ do
      let structType = AT.TStruct "struct" [("field1", AT.TInt 32), ("field2", AT.TInt 32)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "struct" structType (Just (AT.Lit sampleLoc (AT.LStruct [("field1", AT.LInt 0), ("field2", AT.LInt 0)]))),
                  AT.StructAccess sampleLoc (AT.Var sampleLoc "struct" structType) (AT.Var sampleLoc "badField" (AT.TInt 32))
                ]
      let expectedError = CE.CodegenError sampleLoc (CE.StructureFieldNotFound "badField")
      testError expectedError funcExpr

    H.it "should throw error if structure does not exist" $ do
      let structType = AT.TStruct "struct" [("field1", AT.TInt 32), ("field2", AT.TInt 32)]
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.StructAccess sampleLoc (AT.Var sampleLoc "struct" structType) (AT.Var sampleLoc "badField" (AT.TInt 32))
                ]
      let expectedError = CE.CodegenError sampleLoc (CE.VariableNotFound "struct")
      testError expectedError funcExpr

    H.it "should throw error if structure type is not valid " $ do
      let structType = AT.TStruct "struct" [("field1", AT.TInt 32), ("field2", AT.TInt 32)]
      let badStructType = AT.TInt 32
      let funcExpr =
            wrapInFunction $
              AT.Block
                [ AT.Declaration sampleLoc "struct" structType (Just (AT.Lit sampleLoc (AT.LStruct [("field1", AT.LInt 0), ("field2", AT.LInt 0)]))),
                  AT.StructAccess sampleLoc (AT.Var sampleLoc "struct" badStructType) (AT.Var sampleLoc "field1" (AT.TInt 32))
                ]
      let expectedError = CE.CodegenError sampleLoc (CE.UnsupportedStructureAccess (AT.Var sampleLoc "struct" badStructType))
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

    isGepInstr (AST.UnName _ AST.:= AST.GetElementPtr {}) = True
    isGepInstr _ = False

    isLoadInstr (AST.UnName _ AST.:= AST.Load {}) = True
    isLoadInstr _ = False
