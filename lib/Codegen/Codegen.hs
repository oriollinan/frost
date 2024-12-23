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
import qualified LLVM.IRBuilder.Constant as IC
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

-- | Type alias for the local code generation state.
type LocalState = [(String, AST.Operand)]

-- | Type alias for the global code generation state.
type GlobalState = [(String, AST.Operand)]

type LoopState = Maybe (AST.Name, AST.Name)

-- | Combined state for code generation.
data CodegenState = CodegenState
  { localState :: LocalState,
    globalState :: GlobalState,
    loopState :: LoopState
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
  | UnsupportedForDefinition AT.Expr
  | UnsupportedWhileDefinition AT.Expr
  | VariableNotFound String
  | UnsupportedFunctionCall String
  | ContinueOutsideLoop
  | BreakOutsideLoop
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
    AT.TPointer t -> T.ptr (toLLVM t)
    AT.TArray t (Just n) -> T.ArrayType (fromIntegral n) (toLLVM t)
    AT.TArray t Nothing -> T.ptr (toLLVM t)
    _ -> undefined

-- | Generate LLVM code for a program.
codegen :: AT.Program -> Either CodegenError AST.Module
codegen program =
  E.runExcept $
    M.buildModuleT (U.stringToByteString $ AT.sourceFile program) $
      IRM.runIRBuilderT IRM.emptyIRBuilder $
        S.evalStateT (mapM_ (generateGlobal . snd) (AT.globals program)) (CodegenState [] [] Nothing)

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
    AT.ArrayAccess {} -> generateArrayAccess expr
    AT.Cast {} -> generateCast expr
    AT.For {} -> generateForLoop expr
    AT.While {} -> generateWhileLoop expr
    AT.Break {} -> generateBreak expr
    AT.Continue {} -> generateContinue expr
    _ -> E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for constants.
generateConstant :: (MonadCodegen m) => AT.Literal -> m C.Constant
generateConstant lit = case lit of
  AT.LInt n -> return $ C.Int 32 (fromIntegral n)
  AT.LChar c -> return $ C.Int 8 (fromIntegral $ fromEnum c)
  AT.LBool b -> return $ C.Int 1 (if b then 1 else 0)
  AT.LNull -> return $ C.Null T.i8
  AT.LFloat f -> pure $ C.Float (FF.Single (realToFrac f))
  AT.LArray elems -> do
    constants <- mapM generateConstant elems
    return $ C.Array (TD.typeOf $ head constants) constants

-- | Generate LLVM code for literals.
generateLiteral :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateLiteral (AT.Lit _ lit) = do
  constant <- generateConstant lit
  pure $ AST.ConstantOperand constant
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
    BinaryOp AT.Mod I.srem
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
    unaryFunction :: AST.Operand -> IRM.IRBuilderT (M.ModuleBuilderT m) AST.Operand
  }

-- | List of supported unary operators.
unaryOperators :: (MonadCodegen m) => [UnaryOp m]
unaryOperators = undefined

-- | Generate LLVM code for unary operations.
generateUnaryOp :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateUnaryOp = undefined

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

  thenBlock <- IRM.block `IRM.named` U.stringToByteString "ifThen"
  thenValue <- generateExpr then_
  I.br mergeBB

  elseBlock <- IRM.block `IRM.named` U.stringToByteString "ifElse"
  elseValue <- maybe (pure $ AST.ConstantOperand $ C.Undef T.void) generateExpr else_
  I.br mergeBB

  mergeBB <- IRM.block `IRM.named` U.stringToByteString "ifMerge"
  I.phi [(thenValue, thenBlock), (elseValue, elseBlock)]
generateIf expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for function definitions.
generateFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunction = \case
  AT.Function _ name (AT.TFunction ret params False) paramNames body -> do
    let funcName = AST.Name $ U.stringToByteString name
        paramTypes = zipWith mkParam params paramNames
        funcType = T.ptr $ T.FunctionType (toLLVM ret) (map fst paramTypes) False
    addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName
    M.function funcName paramTypes (toLLVM ret) $ \ops -> do
      S.modify (\s -> s {localState = []})
      S.zipWithM_ addVar paramNames ops
      result <- generateExpr body
      I.ret result
  _ -> undefined
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

-- | Generate LLVM code for array access.
generateArrayAccess :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateArrayAccess (AT.ArrayAccess _ (AT.Var _ name _) indexExpr) = do
  maybeVar <- getVar name
  ptr <- case maybeVar of
    Just arrayPtr -> return arrayPtr
    Nothing -> E.throwError $ VariableNotFound name
  index <- generateExpr indexExpr
  elementPtr <- I.gep ptr [IC.int32 0, index]
  I.load elementPtr 0
generateArrayAccess expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for type casts.
generateCast :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateCast (AT.Cast _ typ expr) = do
  operand <- generateExpr expr
  let fromType = TD.typeOf operand
      toType = toLLVM typ
  case (fromType, toType) of
    (T.IntegerType fromBits, T.IntegerType toBits)
      | fromBits < toBits -> I.zext operand toType
    (T.IntegerType fromBits, T.IntegerType toBits)
      | fromBits > toBits -> I.trunc operand toType
    (T.IntegerType _, T.FloatingPointType _) -> I.sitofp operand toType
    (T.FloatingPointType _, T.IntegerType _) -> I.fptosi operand toType
    (T.FloatingPointType _, T.FloatingPointType _) -> I.fptrunc operand toType
    (T.ArrayType _ _, T.PointerType _ _) -> I.bitcast operand toType
    (T.ArrayType _ _, T.ArrayType _ _) -> I.bitcast operand toType
    (T.IntegerType _, T.PointerType _ _) -> I.inttoptr operand toType
    _ -> E.throwError $ UnsupportedType typ
generateCast expr =
  E.throwError $ UnsupportedDefinition expr

-- | Generate LLVM code for for loops.
generateForLoop :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateForLoop (AT.For _ init' cond update body) = mdo
  _ <- generateExpr init'
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "forCond"
  condValue <- generateExpr cond
  I.condBr condValue bodyBlock mergeBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "forBody"
  state <- S.gets loopState
  S.modify (\s -> s {loopState = Just (condBlock, mergeBlock)})
  _ <- generateExpr body
  S.modify (\s -> s {loopState = state})
  I.br stepBlock

  stepBlock <- IRM.block `IRM.named` U.stringToByteString "forStep"
  _ <- generateExpr update
  I.br condBlock

  mergeBlock <- IRM.block `IRM.named` U.stringToByteString "forMerge"
  pure $ AST.ConstantOperand $ C.Undef T.void
generateForLoop expr = E.throwError $ UnsupportedForDefinition expr

-- | Generate LLVM code for while loops.
generateWhileLoop :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateWhileLoop (AT.While _ cond body) = mdo
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "whileCond"
  condValue <- generateExpr cond
  I.condBr condValue bodyBlock mergeBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "whileBody"
  state <- S.gets loopState
  S.modify (\s -> s {loopState = Just (condBlock, mergeBlock)})
  _ <- generateExpr body
  S.modify (\s -> s {loopState = state})
  I.br condBlock

  mergeBlock <- IRM.block `IRM.named` U.stringToByteString "whileMerge"
  pure $ AST.ConstantOperand $ C.Undef T.void
generateWhileLoop expr = E.throwError $ UnsupportedWhileDefinition expr

generateBreak :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBreak (AT.Break _) = do
  state <- S.get
  case loopState state of
    Just (_, breakBlock) -> I.br breakBlock >> pure (AST.ConstantOperand $ C.Undef T.void)
    Nothing -> E.throwError ContinueOutsideLoop
generateBreak expr =
  E.throwError $ UnsupportedDefinition expr

generateContinue :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateContinue (AT.Continue _) = do
  state <- S.get
  case loopState state of
    Just (continueBlock, _) -> I.br continueBlock >> pure (AST.ConstantOperand $ C.Undef T.void)
    Nothing -> E.throwError ContinueOutsideLoop
generateContinue expr =
  E.throwError $ UnsupportedDefinition expr
