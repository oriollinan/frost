{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.Operator where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import {-# SOURCE #-} Codegen.ExprGen.ExprGen (ExprGen (..))
import qualified Codegen.State as CS
import qualified Control.Monad.Except as E
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as FF
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified LLVM.IRBuilder.Instruction as I
import qualified Shared.Utils as SU

-- | Generate LLVM code for binary operations.
generateBinaryOp :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateBinaryOp (AT.Op loc op e1 e2) = do
  v1 <- generateExpr e1
  v2 <- generateExpr e2
  let ty1 = TD.typeOf v1
      ty2 = TD.typeOf v2
  case (ty1, ty2) of
    (T.PointerType _ _, T.IntegerType _) -> case op of
      AT.Add -> I.gep v1 [v2]
      AT.Sub -> do
        negV2 <- I.sub (AST.ConstantOperand $ C.Int 32 0) v2
        I.gep v1 [negV2]
      _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
    (T.IntegerType _, T.IntegerType _) -> case findIntOperator op of
      Just f -> f v1 v2
      Nothing -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
    (T.FloatingPointType _, T.FloatingPointType _) -> case findFloatOperator op of
      Just f -> f v1 v2
      Nothing -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
    _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
  where
    findIntOperator op' = L.find ((== op') . opMapping) integerBinaryOperators >>= Just . opFunction
    findFloatOperator op' = L.find ((== op') . opMapping) floatingPointBinaryOperators >>= Just . opFunction
generateBinaryOp expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Binary operation data type.
data BinaryOp m = BinaryOp
  { opMapping :: AT.Operation,
    opFunction :: AST.Operand -> AST.Operand -> m AST.Operand
  }

-- | List of supported integer binary operators.
integerBinaryOperators :: (CS.MonadCodegen m) => [BinaryOp m]
integerBinaryOperators =
  [ BinaryOp AT.Add I.add,
    BinaryOp AT.Sub I.sub,
    BinaryOp AT.Mul I.mul,
    BinaryOp AT.Div I.sdiv,
    BinaryOp AT.Mod I.srem,
    BinaryOp AT.BitAnd I.and,
    BinaryOp AT.BitOr I.or,
    BinaryOp AT.BitXor I.xor,
    BinaryOp AT.BitShl I.shl,
    BinaryOp AT.BitShr I.ashr,
    BinaryOp AT.And I.and,
    BinaryOp AT.Or I.or
  ]
    ++ map mkComparisonOp comparisonOps
  where
    mkComparisonOp (op, pre) = BinaryOp op (I.icmp pre)
    comparisonOps =
      [ (AT.Lt, IP.SLT),
        (AT.Gt, IP.SGT),
        (AT.Lte, IP.SLE),
        (AT.Gte, IP.SGE),
        (AT.Eq, IP.EQ),
        (AT.Ne, IP.NE)
      ]

-- | List of supported floating-point binary operators.
floatingPointBinaryOperators :: (CS.MonadCodegen m) => [BinaryOp m]
floatingPointBinaryOperators =
  [ BinaryOp AT.Add I.fadd,
    BinaryOp AT.Sub I.fsub,
    BinaryOp AT.Mul I.fmul,
    BinaryOp AT.Div I.fdiv,
    BinaryOp AT.Mod I.frem
  ]
    ++ map mkComparisonOp comparisonOps
  where
    mkComparisonOp (op, pre) = BinaryOp op (I.fcmp pre)
    comparisonOps =
      [ (AT.Lt, FP.OLT),
        (AT.Gt, FP.OGT),
        (AT.Lte, FP.OLE),
        (AT.Gte, FP.OGE),
        (AT.Eq, FP.OEQ),
        (AT.Ne, FP.ONE)
      ]

-- | Unary operation data type.
data UnaryOp m = UnaryOp
  { unaryMapping :: AT.UnaryOperation,
    unaryFunction :: AST.Operand -> m AST.Operand
  }

-- | Generate LLVM code for unary operations.
generateUnaryOp :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateUnaryOp (AT.UnaryOp loc op expr) = do
  operand <- generateExpr expr
  case findOperator op of
    Just f -> f operand
    Nothing -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator op
  where
    findOperator op' = L.find ((== op') . unaryMapping) (unaryOperators loc) >>= Just . unaryFunction
generateUnaryOp expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | List of supported unary operators.
unaryOperators :: (CS.MonadCodegen m) => AT.SrcLoc -> [UnaryOp m]
unaryOperators loc =
  [ UnaryOp AT.PreInc $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          val <- I.load operand 0
          newVal <- I.add val (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure newVal
        T.IntegerType bits -> do
          I.add operand (AST.ConstantOperand $ C.Int bits 1)
        T.FloatingPointType T.FloatFP -> do
          I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
        T.FloatingPointType T.DoubleFP -> do
          I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PreInc,
    UnaryOp AT.PreDec $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          val <- I.load operand 0
          newVal <- I.sub val (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure newVal
        T.IntegerType bits -> do
          I.sub operand (AST.ConstantOperand $ C.Int bits 1)
        T.FloatingPointType T.FloatFP -> do
          I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
        T.FloatingPointType T.DoubleFP -> do
          I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PreDec,
    UnaryOp AT.PostInc $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          oldVal <- I.load operand 0
          newVal <- I.add oldVal (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure oldVal
        T.IntegerType bits -> do
          let oldVal = operand
          _ <- I.add operand (AST.ConstantOperand $ C.Int bits 1)
          pure oldVal
        T.FloatingPointType T.FloatFP -> do
          let oldVal = operand
          _ <- I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
          pure oldVal
        T.FloatingPointType T.DoubleFP -> do
          let oldVal = operand
          _ <- I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
          pure oldVal
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PostInc,
    UnaryOp AT.PostDec $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          oldVal <- I.load operand 0
          newVal <- I.sub oldVal (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure oldVal
        T.IntegerType bits -> do
          let oldVal = operand
          _ <- I.sub operand (AST.ConstantOperand $ C.Int bits 1)
          pure oldVal
        T.FloatingPointType T.FloatFP -> do
          let oldVal = operand
          _ <- I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
          pure oldVal
        T.FloatingPointType T.DoubleFP -> do
          let oldVal = operand
          _ <- I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
          pure oldVal
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PostDec,
    UnaryOp AT.Not $ \operand -> do
      let operandType = TD.typeOf operand
      case operandType of
        T.PointerType _ _ -> do
          I.icmp IP.EQ operand (AST.ConstantOperand $ C.Null operandType)
        T.IntegerType _ -> I.xor operand (AST.ConstantOperand $ C.Int 32 (-1))
        T.FloatingPointType T.FloatFP -> do
          I.fcmp FP.OEQ operand (AST.ConstantOperand $ C.Float $ FF.Single 0.0)
        T.FloatingPointType T.DoubleFP -> do
          I.fcmp FP.OEQ operand (AST.ConstantOperand $ C.Float $ FF.Double 0.0)
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.Not,
    UnaryOp AT.BitNot $ \operand -> do
      case TD.typeOf operand of
        T.IntegerType bits ->
          I.xor operand (AST.ConstantOperand $ C.Int bits (-1))
        T.FloatingPointType T.FloatFP -> do
          intValue <- I.bitcast operand (T.IntegerType 32)
          notted <- I.xor intValue (AST.ConstantOperand $ C.Int 32 (-1))
          I.bitcast notted (T.FloatingPointType T.FloatFP)
        T.FloatingPointType T.DoubleFP -> do
          intValue <- I.bitcast operand (T.IntegerType 64)
          notted <- I.xor intValue (AST.ConstantOperand $ C.Int 64 (-1))
          I.bitcast notted (T.FloatingPointType T.DoubleFP)
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.BitNot,
    UnaryOp AT.Deref $ \operand -> do
      I.load operand 0,
    UnaryOp AT.AddrOf $ \operand -> do
      ptr <- I.alloca (TD.typeOf operand) Nothing 0
      I.store ptr 0 operand
      pure ptr
  ]
