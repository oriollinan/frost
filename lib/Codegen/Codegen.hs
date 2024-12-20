{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Codegen.Codegen where

import qualified Ast.Types as AT
import qualified Codegen.Utils as U
import qualified Control.Monad as CM
import qualified Control.Monad.Except as E
import qualified Control.Monad.Fix as F
import qualified Control.Monad.State as S
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as FF
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

-- | Type alias for the local code generation state.
type LocalState = [(String, AST.Operand)]

-- | Type alias for the global code generation state.
type GlobalState = [(String, AST.Operand)]

-- | Combined state for code generation.
data CodegenState = CodegenState
  { localState :: LocalState,
    globalState :: GlobalState
  }
  deriving (Show)

-- | Monad constraints for code generation.
type MonadCodegen m =
  ( IRM.MonadIRBuilder m,
    M.MonadModuleBuilder m,
    F.MonadFix m,
    S.MonadState CodegenState m,
    E.MonadError CodegenError m
  )

-- | Error types for code generation.
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

-- | Variable binding typeclass.
class (Monad m) => VarBinding m where
  getVar :: String -> m (Maybe AST.Operand)
  addVar :: String -> AST.Operand -> m ()
  getGlobalVar :: String -> m (Maybe AST.Operand)
  addGlobalVar :: String -> AST.Operand -> m ()

instance (MonadCodegen m, Monad m) => VarBinding m where
  getVar name = do
    state <- S.get
    return $ lookup name (localState state) `S.mplus` lookup name (globalState state)
  addVar name operand = S.modify (\s -> s {localState = (name, operand) : localState s})
  getGlobalVar name = S.gets (lookup name . globalState)
  addGlobalVar name operand = S.modify (\s -> s {globalState = (name, operand) : globalState s})

-- | Type conversion to LLVM IR.
class ToLLVM a where
  toLLVM :: a -> T.Type

instance ToLLVM AT.Type where
  toLLVM = \case
    AT.TInt width -> T.IntegerType (fromIntegral width)
    AT.TFloat -> T.FloatingPointType T.FloatFP
    AT.TDouble -> T.FloatingPointType T.DoubleFP
    AT.TChar -> T.IntegerType 8
    AT.TBoolean -> T.IntegerType 1
    AT.TVoid -> T.void
    AT.TMutable t -> toLLVM t
    AT.TPointer t -> T.ptr (toLLVM t)
    AT.TArray t (Just n) -> T.ArrayType (fromIntegral n) (toLLVM t)
    AT.TArray t Nothing -> T.ptr (toLLVM t)
    AT.TFunction ret params var -> T.FunctionType (toLLVM ret) (map toLLVM params) var
    AT.TStruct _ fields -> T.StructureType False (map (toLLVM . snd) fields)
    AT.TUnion _ variants -> T.StructureType False (map (toLLVM . snd) variants)
    AT.TTypedef _ t -> toLLVM t

-- | Generate LLVM code for a program.
codegen :: AT.Program -> Either CodegenError AST.Module
codegen program =
  E.runExcept $
    M.buildModuleT (U.stringToByteString $ AT.sourceFile program) $
      IRM.runIRBuilderT IRM.emptyIRBuilder $
        S.evalStateT (mapM_ (generateGlobal . snd) (AT.globals program)) (CodegenState [] [])

-- | Generate LLVM code for global expressions.
generateGlobal :: (MonadCodegen m) => AT.Expr -> m ()
generateGlobal expr = case expr of
  AT.Function {} -> CM.void $ generateFunction expr
  _ -> E.throwError $ UnsupportedTopLevel expr

-- | Generate LLVM code for an expression.
class ExprGen a where
  generateExpr :: (MonadCodegen m) => a -> m AST.Operand

instance ExprGen AT.Expr where
  generateExpr expr = case expr of
    AT.Lit {} -> generateLiteral expr
    AT.Var {} -> generateVar expr
    AT.Function {} -> generateFunction expr
    AT.Declaration {} -> generateDeclaration expr
    AT.If {} -> generateIf expr
    AT.Block {} -> generateBlock expr
    AT.Return {} -> generateReturn expr
    AT.Op {} -> generateBinaryOp expr
    AT.UnaryOp {} -> generateUnaryOp expr
    AT.Call {} -> generateFunctionCall expr
    AT.Assignment {} -> generateAssignment expr
    AT.While {} -> generateWhile expr
    AT.For {} -> generateFor expr
    AT.Break {} -> generateBreak expr
    AT.Continue {} -> generateContinue expr
    AT.ArrayAccess {} -> generateArrayAccess expr
    AT.Cast {} -> generateCast expr
    _ -> E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for literals.
generateLiteral :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateLiteral (AT.Lit loc lit) =
  mkConstant <$> case lit of
    AT.LInt n -> pure $ C.Int 32 (fromIntegral n)
    AT.LChar c -> pure $ C.Int 8 (fromIntegral $ fromEnum c)
    AT.LBool b -> pure $ C.Int 1 (if b then 1 else 0)
    AT.LFloat f -> pure $ C.Float (FF.Single (realToFrac f))
    AT.LArray lits -> do
      operands <- mapM (generateLiteral . AT.Lit loc) lits
      let constants = map extractConstant operands
      let typ = TD.typeOf $ head operands
      pure $ C.Array typ constants
    AT.LNull -> pure $ C.Null T.void
  where
    extractConstant (AST.ConstantOperand c) = c
    extractConstant _ = error "Expected constant operand"
    mkConstant = AST.ConstantOperand
generateLiteral expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for binary operations.
generateBinaryOp :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBinaryOp (AT.Op _ op e1 e2) = do
  v1 <- generateExpr e1
  v2 <- generateExpr e2
  case findOperator op of
    Just f -> f v1 v2
    Nothing -> E.throwError $ UnsupportedOperator op
  where
    findOperator op' = L.find ((== op') . opMapping) binaryOperators >>= Just . opFunction
generateBinaryOp expr =
  E.throwError $ UnsupportedDefinition expr

-- | Binary operation data type.
data BinaryOp m = BinaryOp
  { opMapping :: AT.Operation,
    opFunction :: AST.Operand -> AST.Operand -> m AST.Operand
  }

-- | List of supported binary operators.
binaryOperators :: (MonadCodegen m) => [BinaryOp m]
binaryOperators =
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

-- | Unary operation data type.
data UnaryOp m = UnaryOp
  { unaryMapping :: AT.UnaryOperation,
    unaryFunction :: AST.Operand -> m AST.Operand
  }

-- | List of supported unary operators.
unaryOperators :: (MonadCodegen m) => [UnaryOp m]
unaryOperators =
  [ UnaryOp AT.Not (\operand -> I.xor operand (AST.ConstantOperand $ C.Int 1 1)),
    UnaryOp AT.BitNot (\operand -> I.xor operand (AST.ConstantOperand $ C.Int 32 (-1))),
    UnaryOp AT.Deref (`I.load` 0),
    UnaryOp AT.AddrOf pure,
    UnaryOp AT.PreInc (\operand -> I.add operand (AST.ConstantOperand $ C.Int 32 1)),
    UnaryOp AT.PreDec (\operand -> I.sub operand (AST.ConstantOperand $ C.Int 32 1)),
    UnaryOp AT.PostInc (postOp I.add),
    UnaryOp AT.PostDec (postOp I.sub)
  ]
  where
    postOp op operand = do
      result <- I.load operand 0
      I.store operand 0 =<< op result (AST.ConstantOperand $ C.Int 32 1)
      pure result

-- | Generate LLVM code for unary operations.
generateUnaryOp :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateUnaryOp (AT.UnaryOp _ op expr) = do
  operand <- generateExpr expr
  case findOperator op of
    Just f -> f operand
    Nothing -> E.throwError $ UnsupportedUnaryOperator op
  where
    findOperator op' = L.find ((== op') . unaryMapping) unaryOperators >>= Just . unaryFunction
generateUnaryOp expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for variable references.
generateVar :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateVar (AT.Var _ name _) = do
  maybeVar <- getVar name
  case maybeVar of
    Just ptr -> case TD.typeOf ptr of
      T.PointerType _ _ -> I.load ptr 0
      _ -> return ptr
    Nothing -> E.throwError $ VariableNotFound name
generateVar expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for blocks.
generateBlock :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBlock (AT.Block exprs) = do
  _ <- IRM.block `IRM.named` U.stringToByteString "block"
  last <$> traverse generateExpr exprs
generateBlock expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for `if` expressions.
generateIf :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateIf (AT.If _ cond then_ else_) = mdo
  condValue <- generateExpr cond
  test <- I.icmp IP.NE condValue (AST.ConstantOperand $ C.Int 1 0)
  I.condBr test thenBlock elseBlock

  thenBlock <- IRM.block `IRM.named` U.stringToByteString "then"
  thenValue <- generateExpr then_
  I.br mergeBB

  elseBlock <- IRM.block `IRM.named` U.stringToByteString "else"
  elseValue <- maybe (pure $ AST.ConstantOperand $ C.Undef T.void) generateExpr else_
  I.br mergeBB

  mergeBB <- IRM.block `IRM.named` U.stringToByteString "merge"
  I.phi [(thenValue, thenBlock), (elseValue, elseBlock)]
generateIf expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for function definitions.
generateFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunction = \case
  AT.Function _ name (AT.TFunction ret params var) paramNames body -> do
    let funcName = AST.Name $ U.stringToByteString name
        paramTypes = zipWith mkParam params paramNames
        funcType = T.ptr $ T.FunctionType (toLLVM ret) (map fst paramTypes) var
    addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName
    M.function funcName paramTypes (toLLVM ret) $ \ops -> do
      S.modify (\s -> s {localState = []})
      S.zipWithM_ addVar paramNames ops
      result <- generateExpr body
      I.ret result
  expr -> E.throwError $ UnsupportedDefinition expr
  where
    mkParam t n = (toLLVM t, M.ParameterName $ U.stringToByteString n)

-- | Generate LLVM code for declarations.
generateDeclaration :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateDeclaration (AT.Declaration _ name typ mInitExpr) = do
  let llvmType = toLLVM typ
  ptr <- I.alloca llvmType Nothing 0
  case mInitExpr of
    Just initExpr -> do
      initValue <- generateExpr initExpr
      I.store ptr 0 initValue
    Nothing -> pure ()
  addVar name ptr
  pure ptr
generateDeclaration expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for return statements.
generateReturn :: (MonadCodegen m) => AT.Expr -> m AST.Operand
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
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for function calls.
generateFunctionCall :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunctionCall (AT.Call _ (AT.Var _ name _) args) = do
  maybeFunc <- getVar name
  case maybeFunc of
    Just funcOperand -> case funcOperand of
      AST.ConstantOperand (C.GlobalReference _ _) -> do
        operandArgs <- mapM generateExpr args
        I.call funcOperand (map (,[]) operandArgs)
      _ -> do
        funcPtr <- I.load funcOperand 0
        operandArgs <- mapM generateExpr args
        I.call funcPtr (map (,[]) operandArgs)
    Nothing -> E.throwError $ UnsupportedFunctionCall name
generateFunctionCall expr =
  E.throwError $ UnsupportedDefinition expr

-- | Check the type of an argument.
checkArgumentType :: (MonadCodegen m) => T.Type -> AT.Expr -> m ()
checkArgumentType expectedType expr = do
  operand <- generateExpr expr
  let actualType = TD.typeOf operand
  CM.when (actualType /= expectedType) $
    E.throwError $
      UnsupportedFunctionCall "Argument type mismatch"

-- | Generate a regular function call (for non-lambda functions).
generateRegularFunctionCall :: (MonadCodegen m) => String -> [AT.Expr] -> m AST.Operand
generateRegularFunctionCall name args = do
  maybeFunc <- getVar name
  case maybeFunc of
    Just funcOperand -> do
      operandArgs <- mapM generateExpr args
      I.call funcOperand (map (,[]) operandArgs)
    Nothing -> E.throwError $ UnsupportedFunctionCall name

-- | Generate LLVM code for assignment expressions.
generateAssignment :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateAssignment (AT.Assignment _ target value) = do
  targetOperand <- generateExpr target
  valueOperand <- generateExpr value
  I.store targetOperand 0 valueOperand
  pure valueOperand
generateAssignment expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for a while loop.
generateWhile :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateWhile (AT.While _ cond body) = mdo
  condBlock <- IRM.block `IRM.named` U.stringToByteString "cond"
  I.br condBlock

  condValue <- generateExpr cond
  test <- I.icmp IP.NE condValue (AST.ConstantOperand $ C.Int 1 0)
  I.condBr test bodyBlock mergeBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "body"
  _ <- generateExpr body
  I.br condBlock

  mergeBlock <- IRM.block `IRM.named` U.stringToByteString "merge"
  pure $ AST.ConstantOperand $ C.Undef T.void
generateWhile expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for a for loop.
generateFor :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFor (AT.For _ ini cond update body) = mdo
  _ <- generateExpr ini
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "cond"
  condValue <- generateExpr cond
  test <- I.icmp IP.NE condValue (AST.ConstantOperand $ C.Int 1 0)
  I.condBr test bodyBlock mergeBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "body"
  _ <- generateExpr body
  I.br updateBlock

  updateBlock <- IRM.block `IRM.named` U.stringToByteString "update"
  _ <- generateExpr update
  I.br condBlock

  mergeBlock <- IRM.block `IRM.named` U.stringToByteString "merge"
  pure $ AST.ConstantOperand $ C.Undef T.void
generateFor expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for a break statement.
generateBreak :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBreak (AT.Break _) = do
  I.br =<< IRM.block `IRM.named` U.stringToByteString "break"
  pure $ AST.ConstantOperand $ C.Undef T.void
generateBreak expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for a continue statement.
generateContinue :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateContinue (AT.Continue _) = do
  I.br =<< IRM.block `IRM.named` U.stringToByteString "continue"
  pure $ AST.ConstantOperand $ C.Undef T.void
generateContinue expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for an array access expression.
generateArrayAccess :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateArrayAccess (AT.ArrayAccess _ array index) = do
  arrayOperand <- generateExpr array
  indexOperand <- generateExpr index
  arrayPtr <- I.gep arrayOperand [indexOperand]
  I.load arrayPtr 0
generateArrayAccess expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for a cast expression.
generateCast :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateCast (AT.Cast _ typ expr) = do
  operand <- generateExpr expr
  let llvmType = toLLVM typ
  I.bitcast operand llvmType
generateCast expr =
  E.throwError $ UnsupportedDefinition expr
