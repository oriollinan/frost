{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.Cast where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import {-# SOURCE #-} Codegen.ExprGen.ExprGen (ExprGen (..))
import qualified Codegen.ExprGen.Types as ET
import qualified Codegen.State as CS
import qualified Control.Monad.Except as E
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified LLVM.IRBuilder.Instruction as I
import qualified Shared.Utils as SU

-- | Generate LLVM code for type casts.
generateCast :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateCast (AT.Cast _ typ expr) = do
  operand <- generateExpr expr
  llvmCast (SU.getLoc expr) operand (TD.typeOf operand) (ET.toLLVM typ)
generateCast expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Cast an operand to match a desired LLVM type.
llvmCast :: (CS.MonadCodegen m) => AT.SrcLoc -> AST.Operand -> T.Type -> T.Type -> m AST.Operand
llvmCast loc operand fromType toType = case (fromType, toType) of
  (T.IntegerType fromBits, T.IntegerType toBits)
    | fromBits < toBits -> I.zext operand toType
    | fromBits > toBits -> I.trunc operand toType
    | otherwise -> pure operand
  (T.FloatingPointType fromFP, T.FloatingPointType toFP)
    | isLargerFP fromFP toFP -> I.fpext operand toType
    | isSmallerFP fromFP toFP -> I.fptrunc operand toType
    | otherwise -> pure operand
  (T.IntegerType _, T.FloatingPointType _) -> I.sitofp operand toType
  (T.FloatingPointType _, T.IntegerType _) -> I.fptosi operand toType
  (x, y) | isBitcastable x y -> I.bitcast operand toType
  _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedConversion fromType toType
  where
    isLargerFP T.FloatFP T.DoubleFP = True
    isLargerFP _ _ = False

    isSmallerFP T.DoubleFP T.FloatFP = True
    isSmallerFP _ _ = False

    isBitcastable (T.PointerType _ _) (T.PointerType _ _) = True
    isBitcastable (T.ArrayType _ _) (T.PointerType _ _) = True
    isBitcastable (T.ArrayType _ _) (T.ArrayType _ _) = True
    isBitcastable (T.PointerType _ _) (T.IntegerType _) = True
    isBitcastable (T.IntegerType _) (T.PointerType _ _) = True
    isBitcastable _ _ = False

-- | Convert an operand to match a desired LLVM type if needed.
ensureMatchingType :: (CS.MonadCodegen m) => AT.SrcLoc -> AST.Operand -> T.Type -> m AST.Operand
ensureMatchingType loc val targetTy
  | TD.typeOf val == targetTy = pure val
  | otherwise = llvmCast loc val (TD.typeOf val) targetTy

-- | Convert an operand to a boolean value.
toBool :: (CS.MonadCodegen m) => AT.SrcLoc -> AST.Operand -> m AST.Operand
toBool loc val = do
  let ty = TD.typeOf val
  case ty of
    T.IntegerType 1 -> pure val
    T.IntegerType 8 -> do
      let zero8 = AST.ConstantOperand (C.Int 8 0)
      I.icmp IP.NE val zero8
    T.IntegerType 32 -> do
      let zero32 = AST.ConstantOperand (C.Int 32 0)
      I.icmp IP.NE val zero32
    _ ->
      E.throwError $ CC.CodegenError loc $ CC.UnsupportedType AT.TVoid
