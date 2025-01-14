{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.Types where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import qualified Codegen.State as CS
import qualified Control.Monad.Except as E
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified LLVM.IRBuilder.Instruction as I

-- | Type conversion to LLVM IR.
class ToLLVM a where
  toLLVM :: a -> T.Type

instance ToLLVM AT.Type where
  toLLVM expr = case expr of
    AT.TInt width -> T.IntegerType (fromIntegral width)
    AT.TFloat -> T.FloatingPointType T.FloatFP
    AT.TDouble -> T.FloatingPointType T.DoubleFP
    AT.TChar -> T.IntegerType 8
    AT.TBoolean -> T.IntegerType 1
    AT.TVoid -> T.void
    AT.TPointer t -> T.ptr (toLLVM t)
    AT.TArray t (Just n) -> T.ArrayType (fromIntegral n) (toLLVM t)
    AT.TArray t Nothing -> T.ptr (toLLVM t)
    AT.TFunction ret params var -> T.FunctionType (toLLVM ret) (map toLLVM params) var
    AT.TStruct _ fields -> T.StructureType False (map (toLLVM . snd) fields)
    AT.TUnion _ variants -> T.StructureType False (map (toLLVM . snd) variants)
    AT.TTypedef _ t -> toLLVM t
    AT.TMutable t -> toLLVM t
    AT.TUnknown -> T.void

-- | Ensure an operand matches a target type, casting it if necessary.
ensureMatchingType :: (CS.MonadCodegen m) => AT.SrcLoc -> AST.Operand -> T.Type -> m AST.Operand
ensureMatchingType loc val targetTy
  | TD.typeOf val == targetTy = pure val
  | otherwise = llvmCast loc val (TD.typeOf val) targetTy

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
  (T.VectorType n fromEl, T.VectorType m toEl)
    | n == m && isBitcastable fromEl toEl -> I.bitcast operand toType
    | n < m -> I.zext operand toType
    | n > m -> I.trunc operand toType
  (T.IntegerType _, T.VectorType _ _) -> I.inttoptr operand toType
  (T.VectorType _ _, T.IntegerType _) -> I.ptrtoint operand toType
  _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedConversion fromType toType
  where
    isLargerFP T.FloatFP T.DoubleFP = True
    isLargerFP T.FloatFP T.X86_FP80FP = True
    isLargerFP T.DoubleFP T.X86_FP80FP = True
    isLargerFP _ _ = False

    isSmallerFP T.DoubleFP T.FloatFP = True
    isSmallerFP T.X86_FP80FP T.DoubleFP = True
    isSmallerFP T.X86_FP80FP T.FloatFP = True
    isSmallerFP _ _ = False

    isBitcastable (T.PointerType _ _) (T.PointerType _ _) = True
    isBitcastable (T.ArrayType _ _) (T.PointerType _ _) = True
    isBitcastable (T.ArrayType _ _) (T.ArrayType _ _) = True
    isBitcastable (T.PointerType _ _) (T.IntegerType _) = True
    isBitcastable (T.IntegerType _) (T.PointerType _ _) = True
    isBitcastable (T.VectorType n fromEl) (T.VectorType m toEl) = n == m && isBitcastable fromEl toEl
    isBitcastable _ _ = False
