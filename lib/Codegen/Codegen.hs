{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.Codegen where

import qualified Ast.Types as AT
import qualified Control.Monad.Fix as Fix
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

-- | Generates LLVM code for a given abstract syntax tree (AST).
-- The `codegen` function takes an AST and returns the corresponding LLVM module.
codegen :: AT.AST -> AST.Module
codegen ast = M.buildModule "generated" $ do
  M.function "$$generated" [] T.i32 $ \_ -> do
    result <- case ast of
      AT.AST exprs -> generateExpr $ head exprs
    I.ret result

-- | Generates an LLVM operand for an expression.
-- The `generateExpr` function recursively processes different expression types
-- and generates the corresponding LLVM code.
generateExpr :: (IRM.MonadIRBuilder m, Fix.MonadFix m) => AT.Expr -> m AST.Operand
generateExpr expr = case expr of
  AT.Lit (AT.LInt n) ->
    pure $ AST.ConstantOperand $ C.Int 32 (fromIntegral n)
  AT.Op AT.Add e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.add v1 v2
  AT.Op AT.Sub e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.sub v1 v2
  AT.Op AT.Mult e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.mul v1 v2
  AT.Op AT.Div e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.sdiv v1 v2
  AT.Op AT.Lt e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.icmp IP.SLT v1 v2
  AT.Op AT.Gt e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.icmp IP.SGT v1 v2
  AT.Op AT.Lte e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.icmp IP.SLE v1 v2
  AT.Op AT.Gte e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.icmp IP.SGE v1 v2
  AT.Op AT.Equal e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.icmp IP.EQ v1 v2
  AT.Op AT.And e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.and v1 v2
  AT.Op AT.Or e1 e2 -> do
    v1 <- generateExpr e1
    v2 <- generateExpr e2
    I.or v1 v2
  AT.If cond then_ else_ -> mdo
    condValue <- generateExpr cond
    test <- I.icmp IP.NE condValue (AST.ConstantOperand $ C.Int 1 0)
    I.condBr test thenBlock elseBlock

    thenBlock <- IRM.block `IRM.named` "then"
    thenValue <- generateExpr then_
    I.br mergeBB

    elseBlock <- IRM.block `IRM.named` "else"
    elseValue <- generateExpr else_
    I.br mergeBB

    mergeBB <- IRM.block `IRM.named` "merge"
    I.phi [(thenValue, thenBlock), (elseValue, elseBlock)]
  _ -> error ("Unimplemented expression type" ++ show expr)
