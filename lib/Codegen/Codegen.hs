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
import qualified Control.Monad.Except as E
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

-- | Type for codegen errors
data CodegenError
  = UnsupportedTopLevel AT.Expr
  | UnsupportedOperator AT.Operation
  | UnsupportedLiteral AT.Literal
  | UnsupportedGlobalVar AT.Literal
  | UnsupportedLocalVar AT.Literal
  | UnsupportedDefinition AT.Expr
  | VariableNotFound String
  | UnsupportedFunctionCall String
  deriving (Show)

-- | Type alias for the monad stack used for code generation.
type MonadCodegen m =
  ( IRM.MonadIRBuilder m,
    M.MonadModuleBuilder m,
    F.MonadFix m,
    S.MonadState CodegenState m,
    E.MonadError CodegenError m
  )

-- | Helper functions to manage state
getVarBinding :: (MonadCodegen m) => String -> m (Maybe AST.Operand)
getVarBinding name = S.gets (lookup name)

-- | Adds a variable binding to the state.
addVarBinding :: (MonadCodegen m) => String -> AST.Operand -> m ()
addVarBinding name op = S.modify ((name, op) :)

-- | Converts a parameter name to a pair of type and parameter name.
-- The `toParamType` function takes a string and returns a pair of type and parameter name.
-- The type is always `i64` (64-bit integer), and the parameter name is the input string.
toParamType :: String -> (T.Type, M.ParameterName)
toParamType param = (T.i64, M.ParameterName $ U.stringToByteString param)

-- | Generates LLVM code for a given abstract syntax tree (AST).
codegen :: AT.AST -> Either CodegenError AST.Module
codegen (AT.AST exprs) =
  E.runExcept $
    M.buildModuleT "$$generated" $
      IRM.runIRBuilderT IRM.emptyIRBuilder $
        S.evalStateT (mapM_ generateTopLevel exprs) []

-- | Generates LLVM code for a top-level expression.
-- The `generateTopLevel` function takes an expression and generates the corresponding LLVM code.
generateTopLevel :: (MonadCodegen m) => AT.Expr -> m ()
generateTopLevel = \case
  AT.Define name (AT.Lit var) -> CM.void $ buildGlobaVariable (AST.mkName name) var
  AT.Define name body -> CM.void $ buildLambda (AST.mkName name) [] body
  expr -> E.throwError $ UnsupportedTopLevel expr

-- | Maps binary operators to LLVM instructions.
binaryOps :: [(AT.Operation, AST.Operand -> AST.Operand -> (IRM.MonadIRBuilder m) => m AST.Operand)]
binaryOps =
  [ (AT.Add, I.add),
    (AT.Sub, I.sub),
    (AT.Mult, I.mul),
    (AT.Div, I.sdiv),
    (AT.Mod, I.srem),
    (AT.Lt, I.icmp IP.SLT),
    (AT.Gt, I.icmp IP.SGT),
    (AT.Lte, I.icmp IP.SLE),
    (AT.Gte, I.icmp IP.SGE),
    (AT.Equal, I.icmp IP.EQ),
    (AT.Ne, I.icmp IP.NE),
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
    Nothing -> E.throwError $ UnsupportedOperator op

-- | Generates LLVM code for a literal variable.
-- The `buildLiteralVariable` function takes the variable name and its value,
buildGlobaVariable :: (MonadCodegen m) => AST.Name -> AT.Literal -> m AST.Operand
buildGlobaVariable name = \case
  AT.LInt i -> M.global name T.i64 (C.Int 64 $ fromIntegral i)
  AT.LBool b -> M.global name T.i64 (C.Int 1 $ if b then 1 else 0)
  value -> E.throwError $ UnsupportedGlobalVar value

-- | Generates LLVM code for a lambda expression.
-- The `buildLambda` function takes a name, a list of parameter names, and a body expression,
-- and returns an LLVM operand representing the lambda function.
buildLambda :: (MonadCodegen m) => AST.Name -> [String] -> AT.Expr -> m AST.Operand
buildLambda name params body = do
  M.function name [toParamType param | param <- params] T.i64 $ \paramOps -> do
    oldState <- S.get
    CM.forM_ (zip params paramOps) $ uncurry addVarBinding
    results <- generateExpr body
    S.put oldState
    I.ret results

-- | Generates an LLVM operand for an expression.
-- The `generateExpr` function recursively processes different expression types
-- and generates the corresponding LLVM code.
generateExpr :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateExpr = \case
  AT.Lit lit -> generateLiteral lit
  AT.Op op e1 e2 -> generateOp op e1 e2
  AT.If cond then_ else_ -> generateIf cond then_ else_
  AT.Call func args -> generateCall func args
  AT.Var name -> generateVar name
  AT.Define name body -> generateDefine name body
  AT.Lambda params body -> generateLambda params body
  AT.Seq exprs -> generateSeq exprs

-- | Generates an LLVM operand for a literal.
-- The `generateLiteral` function takes a literal and returns the corresponding LLVM operand.
generateLiteral :: (MonadCodegen m) => AT.Literal -> m AST.Operand
generateLiteral = \case
  AT.LInt n -> pure $ AST.ConstantOperand $ C.Int 64 (fromIntegral n)
  AT.LBool b -> pure $ AST.ConstantOperand $ C.Int 1 (if b then 1 else 0)
  lit -> E.throwError $ UnsupportedLiteral lit

-- | Generates an LLVM operand for a function call.
-- The `generateCall` function takes a function expression and a list of argument expressions,
-- and returns the corresponding LLVM operand.
generateCall :: (MonadCodegen m) => AT.Expr -> AT.Expr -> m AST.Operand
generateCall func args = do
  func' <- case func of
    AT.Lambda params body -> do
      uniqueName <- IRM.freshName "lambda"
      buildLambda uniqueName params body
    AT.Var name -> do
      maybeOp <- getVarBinding name
      case maybeOp of
        Just op -> pure op
        Nothing -> E.throwError $ VariableNotFound name
    _ -> generateExpr func
  args' <- generateExpr args
  I.call func' [(args', [])]

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
              C.GlobalReference (T.ptr T.i64) (AST.mkName name)
      I.load globalVarPtr 0

-- | Generates an LLVM operand for a definition.
-- The `generateDefine` function takes a variable name and an expression,
-- and returns the corresponding LLVM operand.
generateDefine :: (MonadCodegen m) => String -> AT.Expr -> m AST.Operand
generateDefine name = \case
  AT.Lit var -> case var of
    AT.LInt i -> do
      let op = AST.ConstantOperand (C.Int 64 $ fromIntegral i)
      addVarBinding name op
      generateExpr (AT.Lit var)
    AT.LBool b -> do
      let op = AST.ConstantOperand (C.Int 1 $ if b then 1 else 0)
      addVarBinding name op
      generateExpr (AT.Lit var)
    _ -> E.throwError $ UnsupportedLocalVar var
  AT.Lambda params body -> buildLambda (AST.mkName name) params body
  AT.Var var -> generateVar var
  expr -> E.throwError $ UnsupportedDefinition expr

-- | Generates an LLVM operand for a lambda expression.
-- The `generateLambda` function takes a list of parameter names and a list of body expressions,
-- and returns the corresponding LLVM operand.
generateLambda :: (MonadCodegen m) => [String] -> AT.Expr -> m AST.Operand
generateLambda params body = do
  uniqueName <- IRM.freshName "lambda"
  buildLambda uniqueName params body

-- | Generates an LLVM operand for a sequence of expressions.
-- The `generateSeq` function takes a list of expressions and returns the corresponding LLVM operand.
generateSeq :: (MonadCodegen m) => [AT.Expr] -> m AST.Operand
generateSeq = \case
  [] -> E.throwError $ UnsupportedTopLevel (AT.Seq [])
  [expr] -> generateExpr expr
  exprs -> do
    CM.forM_ (init exprs) generateExpr
    generateExpr (last exprs)
