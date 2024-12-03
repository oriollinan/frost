{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.Codegen where

import qualified Ast.Types as AT
import qualified Control.Monad as CM
import qualified Control.Monad.Fix as Fix
import qualified Data.ByteString.Short as BS
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

-- | Type alias for the monad stack used for code generation.
type MonadCodegen m = (IRM.MonadIRBuilder m, M.MonadModuleBuilder m, Fix.MonadFix m)

-- | Converts a parameter name to a pair of type and parameter name.
-- The `toParamType` function takes a string and returns a pair of type and parameter name.
-- The type is always `i32` (32-bit integer), and the parameter name is the input string.
toParamType :: String -> (T.Type, M.ParameterName)
toParamType param =
  ( T.i32,
    M.ParameterName $ BS.pack $ map (fromIntegral . fromEnum) param
  )

-- | Generates LLVM code for a given abstract syntax tree (AST).
-- The `codegen` function takes an AST and returns the corresponding LLVM module.
codegen :: AT.AST -> AST.Module
codegen (AT.AST exprs) = M.buildModule "$$generated" $ do
  IRM.runIRBuilderT IRM.emptyIRBuilder $ mapM_ generateTopLevel exprs

-- | Generates LLVM code for a top-level expression.
-- The `generateTopLevel` function takes an expression and generates the corresponding LLVM code.
generateTopLevel :: (MonadCodegen m) => AT.Expr -> m ()
generateTopLevel expr = case expr of
  AT.Define name body -> CM.void $ buildLambda (AST.mkName name) [] body
  _ -> error ("Unsupported top-level expression: " ++ show expr)

-- | Maps binary operators to LLVM instructions.
binaryOps :: [(AT.Operation, AST.Operand -> AST.Operand -> (IRM.MonadIRBuilder m) => m AST.Operand)]
binaryOps =
  [ (AT.Add, I.add),
    (AT.Sub, I.sub),
    (AT.Mult, I.mul),
    (AT.Div, I.sdiv),
    (AT.Lt, I.icmp IP.SLT),
    (AT.Gt, I.icmp IP.SGT),
    (AT.Lte, I.icmp IP.SLE),
    (AT.Gte, I.icmp IP.SGE),
    (AT.Equal, I.icmp IP.EQ),
    (AT.And, I.and),
    (AT.Or, I.or)
  ]

-- | Generates LLVM code for an if expression.
generateIf :: (MonadCodegen m) => AT.Expr -> AT.Expr -> AT.Expr -> m AST.Operand
generateIf cond then_ else_ = mdo
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

-- | Generates LLVM code for a binary operation.
generateOp :: (MonadCodegen m) => AT.Operation -> AT.Expr -> AT.Expr -> m AST.Operand
generateOp op e1 e2 = do
  v1 <- generateExpr e1
  v2 <- generateExpr e2
  case lookup op binaryOps of
    Just instruction -> instruction v1 v2
    Nothing -> error $ "Unsupported operator: " ++ show op

-- | Generates LLVM code for a lambda expression.
-- The `buildLambda` function takes a name, a list of parameter names, and a body expression,
-- and returns an LLVM operand representing the lambda function.
buildLambda :: (MonadCodegen m) => AST.Name -> [String] -> AT.Expr -> m AST.Operand
buildLambda name params body = do
  M.function
    name
    [toParamType param | param <- params]
    T.i32
    $ \_ -> do
      result <- generateExpr body
      I.ret result

-- | Generates an LLVM operand for an expression.
-- The `generateExpr` function recursively processes different expression types
-- and generates the corresponding LLVM code.
generateExpr :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateExpr expr = case expr of
  AT.Lit (AT.LInt n) ->
    pure $ AST.ConstantOperand $ C.Int 32 (fromIntegral n)
  AT.Lit (AT.LBool b) ->
    pure $ AST.ConstantOperand $ C.Int 1 (if b then 1 else 0)
  AT.Op op e1 e2 -> generateOp op e1 e2
  AT.If cond then_ else_ -> generateIf cond then_ else_
  AT.Call func args -> do
    func' <- case func of
      AT.Lambda params body -> do
        uniqueName <- IRM.freshName "lambda"
        buildLambda uniqueName params body
      AT.Var name -> pure $ AST.LocalReference T.i32 $ AST.mkName name
      _ -> generateExpr func
    args' <- mapM generateExpr args
    I.call func' [(arg, []) | arg <- args']
  AT.Var name -> pure $ AST.LocalReference T.i32 $ AST.mkName name
  _ -> error ("Unimplemented expression type: " ++ show expr)
