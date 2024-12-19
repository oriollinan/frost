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
import qualified Data.Maybe as MB
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
  | UnsupportedUnaryOperator AT.UnaryOperation
  | UnsupportedLiteral AT.Literal
  | UnsupportedType AT.Type
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

-- | Converts an AST type to an LLVM type.
toLLVMType :: AT.Type -> T.Type
toLLVMType = \case
  AT.TInt width -> T.IntegerType (fromIntegral width)
  AT.TFloat -> T.FloatingPointType T.FloatFP
  AT.TDouble -> T.FloatingPointType T.DoubleFP
  AT.TChar -> T.IntegerType 8
  AT.TBoolean -> T.IntegerType 1
  AT.TVoid -> T.void
  AT.TPointer t -> T.ptr (toLLVMType t)
  AT.TArray t (Just size) -> T.ArrayType (fromIntegral size) (toLLVMType t)
  AT.TArray t Nothing -> T.ptr (toLLVMType t)
  _ -> undefined

-- | Generates LLVM code for a given abstract syntax tree (AST).
codegen :: AT.Program -> Either CodegenError AST.Module
codegen program =
  E.runExcept $
    M.buildModuleT (U.stringToByteString $ AT.sourceFile program) $
      IRM.runIRBuilderT IRM.emptyIRBuilder $
        S.evalStateT (mapM_ (generateGlobal . snd) (AT.globals program)) []

-- | Generates LLVM code for a global variable.
generateGlobal :: (MonadCodegen m) => AT.Expr -> m ()
generateGlobal = \case
  AT.Function loc name typ params body ->
    CM.void $
      generateFunction (AT.Function loc name typ params body)
  expr -> E.throwError $ UnsupportedTopLevel expr

-- | Generates LLVM code for an expression.
generateExpr :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateExpr = \case
  AT.Lit _ lit -> generateLiteral lit
  AT.Var _ name _typ -> generateVar name
  AT.Function loc name typ params body -> generateFunction (AT.Function loc name typ params body)
  AT.Declaration loc name typ ini -> generateDeclaration (AT.Declaration loc name typ ini)
  AT.If loc cond then_ else_ -> generateIf cond then_ (MB.fromMaybe (AT.Lit loc (AT.LInt 0)) else_)
  AT.Block exprs -> generateBlock exprs
  AT.Return loc expr -> generateReturn (AT.Return loc expr)
  AT.Op _ op e1 e2 -> generateOp op e1 e2
  AT.UnaryOp _ op e -> generateUnaryOp op e
  _ -> undefined

-- | Generates LLVM code for a literal.
generateLiteral :: (MonadCodegen m) => AT.Literal -> m AST.Operand
generateLiteral = \case
  AT.LInt n -> pure $ AST.ConstantOperand $ C.Int 64 (fromIntegral n)
  AT.LChar c -> pure $ AST.ConstantOperand $ C.Int 8 (fromIntegral $ fromEnum c)
  AT.LBool b -> pure $ AST.ConstantOperand $ C.Int 1 (if b then 1 else 0)
  AT.LArray _ -> undefined
  AT.LNull -> undefined
  _ -> undefined

-- | Maps binary operators to LLVM instructions.
binaryOps :: [(AT.Operation, AST.Operand -> AST.Operand -> (IRM.MonadIRBuilder m) => m AST.Operand)]
binaryOps =
  [ (AT.Add, I.add),
    (AT.Sub, I.sub),
    (AT.Mul, I.mul),
    (AT.Div, I.sdiv),
    (AT.Mod, I.srem),
    (AT.Lt, I.icmp IP.SLT),
    (AT.Gt, I.icmp IP.SGT),
    (AT.Lte, I.icmp IP.SLE),
    (AT.Gte, I.icmp IP.SGE),
    (AT.Eq, I.icmp IP.EQ),
    (AT.Ne, I.icmp IP.NE),
    (AT.And, I.and),
    (AT.Or, I.or)
  ]

-- | Generates LLVM code for a binary operation.
generateOp :: (MonadCodegen m) => AT.Operation -> AT.Expr -> AT.Expr -> m AST.Operand
generateOp op e1 e2 = do
  v1 <- generateExpr e1
  v2 <- generateExpr e2
  case lookup op binaryOps of
    Just instruction -> instruction v1 v2
    Nothing -> E.throwError $ UnsupportedOperator op

-- | Maps unary operators to LLVM instructions.
unaryOps :: [(AT.UnaryOperation, AST.Operand -> (IRM.MonadIRBuilder m) => m AST.Operand)]
unaryOps = undefined

-- | Generates LLVM code for a unary operation.
generateUnaryOp :: (MonadCodegen m) => AT.UnaryOperation -> AT.Expr -> m AST.Operand
generateUnaryOp = undefined

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

-- | Generates an LLVM operand for a variable.
-- The `generateVar` function takes a variable name and returns the corresponding LLVM operand.
generateVar :: (MonadCodegen m) => String -> m AST.Operand
generateVar name = do
  var <- getVarBinding name
  case var of
    Just ptr -> I.load ptr 0
    Nothing -> E.throwError $ VariableNotFound name

-- | Generates LLVM code for a block expression.
-- The `generateBlock` function takes a list of expressions and returns the corresponding LLVM operand.
generateBlock :: (MonadCodegen m) => [AT.Expr] -> m AST.Operand
generateBlock exprs = do
  _ <- IRM.block `IRM.named` "block"
  results <- mapM generateExpr exprs
  pure $ last results

-- | Generates LLVM code for a function.
-- The `generateFunction` function takes a function expression and returns the corresponding LLVM operand.
generateFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunction (AT.Function _loc name (AT.TFunction ret params False) names body) = do
  let funcName = AST.Name (U.stringToByteString name)
  let paramTypes =
        zipWith
          (\param name' -> (toLLVMType param, M.ParameterName (U.stringToByteString name')))
          params
          names

  M.function funcName paramTypes (toLLVMType ret) $ \_ -> do
    result <- generateExpr body
    I.ret result
generateFunction expr = E.throwError $ UnsupportedTopLevel expr

-- | Checks if a type is a pointer type.
isPointerType :: AT.Type -> Bool
isPointerType (AT.TPointer _) = True
isPointerType _ = False

-- | Generates LLVM code for a declaration.
-- The `generateDeclaration` function takes a declaration expression and returns the corresponding LLVM operand.
generateDeclaration :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateDeclaration (AT.Declaration _ name typ (Just initExpr)) = do
  let llvmType = toLLVMType typ
  ptr <- I.alloca llvmType Nothing 0
  initValue <- generateExpr initExpr
  I.store ptr 0 initValue
  addVarBinding name ptr
  pure ptr
generateDeclaration (AT.Declaration _ name typ Nothing) = do
  let llvmType = toLLVMType typ
  ptr <- I.alloca llvmType Nothing 0
  addVarBinding name ptr
  pure ptr
generateDeclaration expr = E.throwError $ UnsupportedDefinition expr

-- | Generates LLVM code for a return expression.
-- The `generateReturn` function takes an expression and returns the corresponding LLVM operand.
generateReturn :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateReturn (AT.Return _ expr) = do
  result <- case expr of
    Just e -> generateExpr e
    Nothing -> pure $ AST.ConstantOperand $ C.Undef T.void
  case expr of
    Just _ -> I.ret result >> pure result
    Nothing -> I.retVoid >> pure result
generateReturn expr = E.throwError $ UnsupportedDefinition expr
