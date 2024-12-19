{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen.Codegen where

import qualified Ast.Types as AT
import qualified Codegen.Utils as U
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
generateGlobal expr = case expr of
  AT.Function _ name typ params body -> undefined
  left -> E.throwError . UnsupportedTopLevel $ left

-- | Generates LLVM code for an expression.
generateExpr :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateExpr = \case
  AT.Lit loc lit -> generateLiteral lit
  AT.Var loc name typ -> generateVar name
  AT.Function loc name typ params body -> undefined
  AT.Declaration loc name typ init -> undefined
  AT.Assignment loc target val -> undefined
  AT.Call loc func args -> undefined
  AT.If loc cond then_ else_ -> generateIf cond then_ (MB.fromMaybe (AT.Lit loc (AT.LInt 0)) else_)
  AT.While loc cond body -> undefined
  AT.For loc init cond step body -> undefined
  AT.Block exprs -> generateBlock exprs
  AT.Return loc expr -> undefined
  AT.Break loc -> undefined
  AT.Continue loc -> undefined
  AT.Op loc op e1 e2 -> generateOp op e1 e2
  AT.UnaryOp loc op e -> generateUnaryOp op e
  AT.StructAccess loc expr field -> undefined
  AT.ArrayAccess loc arr idx -> undefined
  AT.Cast loc typ expr -> undefined

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
generateVar = undefined

-- | Generates LLVM code for a block expression.
-- The `generateBlock` function takes a list of expressions and returns the corresponding LLVM operand.
generateBlock :: (MonadCodegen m) => [AT.Expr] -> m AST.Operand
generateBlock = undefined
