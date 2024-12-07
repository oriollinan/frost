{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.Codegen where

import qualified Ast.Types as AT
import qualified Codegen.Utils as U
import qualified Control.Monad as CM
import qualified Control.Monad.Fix as F
import qualified Control.Monad.State as S
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

-- | Type alias for the code generation state.
type CodegenState = [(String, AST.Operand)]

-- | Type alias for the monad stack used for code generation.
type MonadCodegen m =
  ( IRM.MonadIRBuilder m,
    M.MonadModuleBuilder m,
    F.MonadFix m,
    S.MonadState CodegenState m
  )

-- | Helper functions to manage state
getVarBinding :: (MonadCodegen m) => String -> m (Maybe AST.Operand)
getVarBinding name = S.gets (lookup name)

-- | Adds a variable binding to the state.
addVarBinding :: (MonadCodegen m) => String -> AST.Operand -> m ()
addVarBinding name op = S.modify ((name, op) :)

-- | Converts a parameter name to a pair of type and parameter name.
-- The `toParamType` function takes a string and returns a pair of type and parameter name.
-- The type is always `i32` (32-bit integer), and the parameter name is the input string.
toParamType :: String -> (T.Type, M.ParameterName)
toParamType param = (T.i32, M.ParameterName $ U.stringToByteString param)

-- | Generates LLVM code for a given abstract syntax tree (AST).
-- The `codegen` function takes an AST and returns the corresponding LLVM module.
codegen :: AT.AST -> AST.Module
codegen (AT.AST exprs) =
  M.buildModule "$$generated" $
    IRM.runIRBuilderT IRM.emptyIRBuilder $
      S.evalStateT (mapM_ generateTopLevel exprs) []

-- | Generates LLVM code for a top-level expression.
-- The `generateTopLevel` function takes an expression and generates the corresponding LLVM code.
generateTopLevel :: (MonadCodegen m) => AT.Expr -> m ()
generateTopLevel expr = case expr of
  AT.Define name (AT.Lit var) -> CM.void $ buildGlobaVariable (AST.mkName name) var
  AT.Define name body -> CM.void $ buildLambda (AST.mkName name) [] [body]
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

-- | Generates LLVM code for a literal variable.
-- The `buildLiteralVariable` function takes the variable name and its value,
buildGlobaVariable :: (MonadCodegen m) => AST.Name -> AT.Literal -> m AST.Operand
buildGlobaVariable name value = do
  let constant = case value of
        AT.LInt i -> C.Int 32 (fromIntegral i)
        AT.LBool b -> C.Int 1 (if b then 1 else 0)
        _ -> error ("Global variable cannot be created with value: " ++ show value)
  M.global name T.i32 constant

-- | Generates LLVM code for a lambda expression.
-- The `buildLambda` function takes a name, a list of parameter names, and a body expression,
-- and returns an LLVM operand representing the lambda function.
buildLambda :: (MonadCodegen m) => AST.Name -> [String] -> [AT.Expr] -> m AST.Operand
buildLambda name params body = do
  M.function name [toParamType param | param <- params] T.i32 $ \paramOps -> do
    oldState <- S.get
    CM.forM_ (zip params paramOps) $ uncurry addVarBinding
    results <- mapM generateExpr body
    S.put oldState
    CM.when (null results) I.retVoid
    I.ret $ last results

-- | Generates an LLVM operand for an expression.
-- The `generateExpr` function recursively processes different expression types
-- and generates the corresponding LLVM code.
generateExpr :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateExpr expr = case expr of
  AT.Lit lit -> generateLiteral lit
  AT.Op op e1 e2 -> generateOp op e1 e2
  AT.If cond then_ else_ -> generateIf cond then_ else_
  AT.Call func args -> generateCall func args
  AT.Var name -> generateVar name
  AT.Define name body -> generateDefine name body
  AT.Lambda params body -> generateLambda params body

-- | Generates an LLVM operand for a literal.
-- The `generateLiteral` function takes a literal and returns the corresponding LLVM operand.
generateLiteral :: (MonadCodegen m) => AT.Literal -> m AST.Operand
generateLiteral =
  pure . AST.ConstantOperand . \case
    AT.LInt n -> C.Int 32 (fromIntegral n)
    AT.LBool b -> C.Int 1 (if b then 1 else 0)
    _ -> error "Unsupported literal type"

-- | Generates an LLVM operand for a function call.
-- The `generateCall` function takes a function expression and a list of argument expressions,
-- and returns the corresponding LLVM operand.
generateCall :: (MonadCodegen m) => AT.Expr -> [AT.Expr] -> m AST.Operand
generateCall func args = do
  func' <- case func of
    AT.Lambda params body -> do
      uniqueName <- IRM.freshName "lambda"
      buildLambda uniqueName params body
    AT.Var name ->
      error $ "Calling a variable as a function is not supported yet: " ++ name
    _ -> generateExpr func
  args' <- mapM generateExpr args
  I.call func' [(arg, []) | arg <- args']

-- | Generates an LLVM operand for a variable.
-- The `generateVar` function takes a variable name and returns the corresponding LLVM operand.
generateVar :: (MonadCodegen m) => String -> m AST.Operand
generateVar name = do
  maybeOp <- getVarBinding name
  case maybeOp of
    Just op -> pure op
    Nothing -> do
      let globalVarPtr =
            AST.ConstantOperand $
              C.GlobalReference (T.ptr T.i32) (AST.mkName name)
      I.load globalVarPtr 0

-- | Generates an LLVM operand for a definition.
-- The `generateDefine` function takes a variable name and an expression,
-- and returns the corresponding LLVM operand.
generateDefine :: (MonadCodegen m) => String -> AT.Expr -> m AST.Operand
generateDefine name = \case
  AT.Lit var -> do
    let constant = case var of
          AT.LInt i -> C.Int 32 (fromIntegral i)
          AT.LBool b -> C.Int 1 (if b then 1 else 0)
          _ -> error ("Local variable cannot be created with value: " ++ show var)
    let op = AST.ConstantOperand constant
    addVarBinding name op
    generateExpr (AT.Lit var)
  expr -> error ("Unsupported expression in definition: " ++ show expr)

-- | Generates an LLVM operand for a lambda expression.
-- The `generateLambda` function takes a list of parameter names and a list of body expressions,
-- and returns the corresponding LLVM operand.
generateLambda :: (MonadCodegen m) => [String] -> [AT.Expr] -> m AST.Operand
generateLambda params body = do
  uniqueName <- IRM.freshName "lambda"
  buildLambda uniqueName params body
