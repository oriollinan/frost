{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.ExprGen.ControlFlow where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import qualified Codegen.ExprGen.Cast as CC
import {-# SOURCE #-} Codegen.ExprGen.ExprGen (ExprGen (..))
import qualified Codegen.State as CS
import qualified Codegen.Utils as U
import qualified Control.Monad as CM
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Monad as IRM
import qualified Shared.Utils as SU

-- | Generate LLVM code for `if` expressions.
generateIf :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateIf (AT.If _ cond thenExpr elseExpr) = mdo
  condVal <- generateExpr cond
  condTest <- I.icmp IP.NE condVal (AST.ConstantOperand $ C.Int 1 0)
  I.condBr condTest thenBlock elseBlock

  thenBlock <- IRM.block `IRM.named` U.stringToByteString "if.then"
  thenVal <- generateExpr thenExpr
  thenTerminated <- IRM.hasTerminator
  _ <- S.unless thenTerminated $ I.br mergeBlock
  thenBlockName <- IRM.currentBlock

  elseBlock <- IRM.block `IRM.named` U.stringToByteString "if.else"
  elseVal <- case elseExpr of
    Just e -> generateExpr e
    Nothing -> pure (AST.ConstantOperand $ C.Undef T.void)
  elseTerminated <- IRM.hasTerminator
  S.unless elseTerminated $ I.br mergeBlock
  elseBlockName <- IRM.currentBlock

  mergeBlock <- IRM.block `IRM.named` U.stringToByteString "if.merge"
  let validBrs =
        [(thenVal, thenBlockName) | not thenTerminated]
          ++ [(elseVal, elseBlockName) | not elseTerminated]
  case validBrs of
    [] -> pure $ AST.ConstantOperand (C.Undef T.void)
    [(v, _)] -> pure v
    _ -> I.phi validBrs
generateIf expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for for loops.
generateFromLoop :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateFromLoop (AT.From loc _ endExpr stepExpr declExpr@(AT.Declaration _ varName varType _) bodyExpr) = mdo
  _ <- generateExpr declExpr
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "for.cond"
  condVal <- generateExpr $ AT.Op loc AT.Gt endExpr zeroExpr
  boolCondVal <- CC.toBool loc condVal
  I.condBr boolCondVal forwardBlock backwardBlock

  forwardBlock <- IRM.block `IRM.named` U.stringToByteString "for.cond.forward"
  forwardVal <- generateExpr $ AT.Op loc AT.Lt varExpr endExpr
  boolForwardVal <- CC.toBool loc forwardVal
  I.condBr boolForwardVal bodyBlock exitBlock

  backwardBlock <- IRM.block `IRM.named` U.stringToByteString "for.cond.backward"
  backwardVal <- generateExpr $ AT.Op loc AT.Gt varExpr endExpr
  boolBackwardVal <- CC.toBool loc backwardVal
  I.condBr boolBackwardVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "for.body"
  oldLoopState <- S.gets CS.loopState
  S.modify (\s -> s {CS.loopState = Just (stepBlock, exitBlock)})

  _ <- generateExpr bodyExpr

  S.modify (\s -> s {CS.loopState = oldLoopState})
  I.br stepBlock

  stepBlock <- IRM.block `IRM.named` U.stringToByteString "for.step"
  _ <- generateExpr stepExpr
  I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "for.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
  where
    varExpr = AT.Var loc varName varType
    zeroExpr = AT.Lit loc (AT.LInt 0)
generateFromLoop expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedForDefinition expr

-- | Generate LLVM code for while loops.
generateWhileLoop :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateWhileLoop (AT.While loc condExpr bodyExpr) = mdo
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "while.cond"
  condVal <- generateExpr condExpr
  boolCondVal <- CC.toBool loc condVal
  I.condBr boolCondVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "while.body"
  oldLoopState <- S.gets CS.loopState
  S.modify (\s -> s {CS.loopState = Just (condBlock, exitBlock)})

  _ <- generateExpr bodyExpr

  S.modify (\s -> s {CS.loopState = oldLoopState})
  bodyTerminated <- IRM.hasTerminator
  CM.unless bodyTerminated $ I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "while.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
generateWhileLoop expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedWhileDefinition expr

-- | Generate LLVM code for break statements.
generateBreak :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateBreak (AT.Break loc) = do
  state <- S.get
  case CS.loopState state of
    Just (_, breakBlock) -> do
      I.br breakBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CC.CodegenError loc CC.BreakOutsideLoop
generateBreak expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for continue statements.
generateContinue :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateContinue (AT.Continue loc) = do
  state <- S.get
  case CS.loopState state of
    Just (continueBlock, _) -> do
      I.br continueBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CC.CodegenError loc CC.ContinueOutsideLoop
generateContinue expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for return statements.
generateReturn :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateReturn (AT.Return _ mExpr) = do
  case mExpr of
    Just expr -> do
      result <- generateExpr expr
      I.ret result
      pure result
    Nothing -> do
      I.retVoid
      pure $ AST.ConstantOperand $ C.Undef T.void
generateReturn expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for blocks.
generateBlock :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateBlock (AT.Block []) = pure $ AST.ConstantOperand $ C.Undef T.void
generateBlock (AT.Block exprs) = do
  last <$> traverse generateExpr exprs
generateBlock expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr
