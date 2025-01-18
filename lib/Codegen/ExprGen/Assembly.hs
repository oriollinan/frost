{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.Assembly where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import {-# SOURCE #-} qualified Codegen.ExprGen.ExprGen as EG
import qualified Codegen.ExprGen.Types as ET
import qualified Codegen.State as CS
import qualified Control.Monad.Except as E
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Short as BS
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as ACC
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.InlineAssembly as IA
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder as IRB
import qualified LLVM.IRBuilder.Monad as IRM
import qualified Shared.Utils as SU

-- | Low level function to generate LLVM code for inline assembly.
-- LLVM's IRM module does not provide a function to generate inline assembly
-- so we have to use the IRBuilder directly.
callInlineAssembly ::
  (IRM.MonadIRBuilder m) =>
  IA.InlineAssembly ->
  AST.Type ->
  [(AST.Operand, [A.ParameterAttribute])] ->
  m AST.Operand
callInlineAssembly asm retType args' = do
  let callInstr =
        AST.Call
          { AST.tailCallKind = Nothing,
            AST.callingConvention = ACC.C,
            AST.returnAttributes = [],
            AST.function = Left asm,
            AST.arguments = args',
            AST.functionAttributes = [],
            AST.metadata = []
          }
  case retType of
    T.VoidType -> do
      IRB.emitInstrVoid callInstr
      pure (AST.ConstantOperand (C.Undef T.void))
    _ -> IRB.emitInstr retType callInstr

-- | Generate LLVM code for assembly expressions.
generateAssembly :: (CS.MonadCodegen m, EG.ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateAssembly (AT.Assembly _ asmExpr) = do
  let llvmRetTy = ET.toLLVM $ AT.asmReturnType asmExpr
      inlineType = T.FunctionType llvmRetTy (map ET.toLLVM $ AT.asmParameters asmExpr) False

      (output, inputs) =
        ( AT.outputConstraint $ AT.asmConstraints asmExpr,
          AT.inputConstraints $ AT.asmConstraints asmExpr
        )
      combinedConstraints = case (output, inputs) of
        ("", []) -> ""
        ("", is) -> L.intercalate "," is
        (o, []) -> "=" ++ o
        (o, is) -> "=" ++ o ++ "," ++ L.intercalate "," is

      dialect = case AT.asmDialect asmExpr of
        AT.Intel -> IA.IntelDialect
        AT.ATT -> IA.ATTDialect

      inlAsm =
        IA.InlineAssembly
          { IA.type' = inlineType,
            IA.assembly = B.pack $ AT.asmCode asmExpr,
            IA.constraints = BS.toShort $ B.pack combinedConstraints,
            IA.hasSideEffects = AT.asmSideEffects asmExpr,
            IA.alignStack = AT.asmAlignStack asmExpr,
            IA.dialect = dialect
          }

  asmOperands <-
    mapM
      ( \argExpr -> do
          argOp <- EG.generateExpr argExpr
          pure (argOp, [])
      )
      (AT.asmArgs asmExpr)

  callInlineAssembly inlAsm llvmRetTy asmOperands
generateAssembly expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr
