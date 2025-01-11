{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
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
import qualified Data.Maybe as M
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

-- | Type alias for the loop code generation state.
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
  = CodegenError
  { errorLoc :: AT.SrcLoc,
    errorType :: CodegenErrorType
  }

data CodegenErrorType
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
  | UnsupportedStructureAccess AT.Expr
  | ContinueOutsideLoop
  | BreakOutsideLoop
  deriving (Show)

instance Show CodegenError where
  show (CodegenError loc err) =
    AT.srcFile loc
      ++ ":"
      ++ show (AT.srcLine loc)
      ++ ":"
      ++ show (AT.srcCol loc)
      ++ ": "
      ++ showErrorType err

showErrorType :: CodegenErrorType -> String
showErrorType err = case err of
  UnsupportedTopLevel expr -> "Unsupported top-level expression: " ++ show expr
  UnsupportedOperator op -> "Unsupported operator: " ++ show op
  UnsupportedUnaryOperator op -> "Unsupported unary operator: " ++ show op
  UnsupportedLiteral lit -> "Unsupported literal: " ++ show lit
  UnsupportedType typ -> "Unsupported type: " ++ show typ
  UnsupportedGlobalVar lit -> "Unsupported global variable: " ++ show lit
  UnsupportedLocalVar lit -> "Unsupported local variable: " ++ show lit
  UnsupportedDefinition expr -> "Unsupported definition: " ++ show expr
  UnsupportedForDefinition expr -> "Invalid for loop: " ++ show expr
  UnsupportedWhileDefinition expr -> "Invalid while loop: " ++ show expr
  VariableNotFound name -> "Variable not found: " ++ name
  UnsupportedFunctionCall name -> "Invalid function call: " ++ name
  UnsupportedStructureAccess expr -> "Invalid structure access: " ++ show expr
  ContinueOutsideLoop -> "Continue statement outside loop"
  BreakOutsideLoop -> "Break statement outside loop"

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
  toLLVM expr = case expr of
    AT.TInt width -> T.IntegerType (fromIntegral width)
    AT.TFloat -> T.FloatingPointType T.FloatFP
    AT.TDouble -> T.FloatingPointType T.DoubleFP
    AT.TChar -> T.IntegerType 8
    AT.TBoolean -> T.IntegerType 1
    AT.TVoid -> T.void
    AT.TPointer t -> T.ptr (toLLVM t)
    AT.TArray t (Just n) -> T.ArrayType (fromIntegral n) (toLLVM t)
    AT.TArray t Nothing -> T.ptr (toLLVM t)
    AT.TFunction ret params var -> T.FunctionType (toLLVM ret) (map toLLVM params) var
    AT.TStruct _ fields -> T.StructureType False (map (toLLVM . snd) fields)
    AT.TUnion _ variants -> T.StructureType False (map (toLLVM . snd) variants)
    AT.TTypedef _ t -> toLLVM t
    AT.TMutable t -> toLLVM t

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
  AT.ForeignFunction {} -> CM.void $ generateForeignFunction expr
  _ -> E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedTopLevel expr

-- | Generate LLVM code for an expression.
class ExprGen a where
  generateExpr :: (MonadCodegen m) => a -> m AST.Operand

instance ExprGen AT.Expr where
  generateExpr expr = case expr of
    AT.Lit {} -> generateLiteral expr
    AT.Var {} -> generateVar expr
    AT.Function {} -> generateFunction expr
    AT.ForeignFunction {} -> generateForeignFunction expr
    AT.Declaration {} -> generateDeclaration expr
    AT.If {} -> generateIf expr
    AT.Block {} -> generateBlock expr
    AT.Return {} -> generateReturn expr
    AT.Op {} -> generateBinaryOp expr
    AT.UnaryOp {} -> generateUnaryOp expr
    AT.Call {} -> generateFunctionCall expr
    AT.ArrayAccess {} -> generateArrayAccess expr
    AT.StructAccess {} -> generateStructAccess expr
    AT.Cast {} -> generateCast expr
    AT.For {} -> generateForLoop expr
    AT.While {} -> generateWhileLoop expr
    AT.Break {} -> generateBreak expr
    AT.Continue {} -> generateContinue expr
    AT.Assignment {} -> generateAssignment expr

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
  AT.LStruct fields -> do
    -- We do not need the names of the fields
    -- as we only use them when accessing the fields of the struct
    let (_, values) = unzip fields
    constants <- mapM generateConstant values
    return $ C.Struct Nothing False constants

-- | Generate LLVM code for literals.
generateLiteral :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateLiteral (AT.Lit _ lit) = do
  constant <- generateConstant lit
  pure $ AST.ConstantOperand constant
generateLiteral expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for binary operations.
generateBinaryOp :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBinaryOp (AT.Op loc op e1 e2) = do
  v1 <- generateExpr e1
  v2 <- generateExpr e2
  case findOperator op of
    Just f -> f v1 v2
    Nothing -> E.throwError $ CodegenError loc $ UnsupportedOperator op
  where
    findOperator op' = L.find ((== op') . opMapping) binaryOperators >>= Just . opFunction
generateBinaryOp expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

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
    Nothing -> E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedUnaryOperator op
  where
    findOperator op' = L.find ((== op') . unaryMapping) unaryOperators >>= Just . unaryFunction
generateUnaryOp expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for variable references.
generateVar :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateVar (AT.Var loc name _) = do
  maybeVar <- getVar name
  case maybeVar of
    Just ptr -> case TD.typeOf ptr of
      T.PointerType _ _ -> I.load ptr 0
      _ -> return ptr
    Nothing -> E.throwError $ CodegenError loc $ VariableNotFound name
generateVar expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for blocks.
generateBlock :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBlock (AT.Block exprs) = do
  last <$> traverse generateExpr exprs
generateBlock expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for `if` expressions.
generateIf :: (MonadCodegen m) => AT.Expr -> m AST.Operand
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
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for function definitions.
generateFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunction (AT.Function _ name (AT.TFunction ret params False) paramNames body) = do
  let funcName = AST.Name $ U.stringToByteString name
      paramTypes = zipWith mkParam params paramNames
      funcType = T.ptr $ T.FunctionType (toLLVM ret) (map fst paramTypes) False
  addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName
  M.function funcName paramTypes (toLLVM ret) $ \ops -> do
    S.modify (\s -> s {localState = []})
    S.zipWithM_ addVar paramNames ops
    result <- generateExpr body
    I.ret result
  where
    mkParam t n = (toLLVM t, M.ParameterName $ U.stringToByteString n)
generateFunction expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for foreign function definitions.
generateForeignFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateForeignFunction (AT.ForeignFunction _ name (AT.TFunction ret params False)) = do
  let funcType = T.ptr $ T.FunctionType (toLLVM ret) (map toLLVM params) False
      funcName = AST.Name $ U.stringToByteString name

  _ <- M.extern funcName (map toLLVM params) (toLLVM ret)

  addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName

  pure $ AST.ConstantOperand $ C.GlobalReference funcType funcName
generateForeignFunction expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

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
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

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
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for function calls.
generateFunctionCall :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunctionCall (AT.Call loc (AT.Var _ name _) args) = do
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
    Nothing -> E.throwError $ CodegenError loc $ UnsupportedFunctionCall name
generateFunctionCall expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Check the type of an argument.
checkArgumentType :: (MonadCodegen m) => T.Type -> AT.Expr -> m ()
checkArgumentType expectedType expr = do
  operand <- generateExpr expr
  let actualType = TD.typeOf operand
  CM.when (actualType /= expectedType) $
    E.throwError $
      CodegenError (U.getLoc expr) $
        UnsupportedFunctionCall "Argument type mismatch"

-- | Generate LLVM code for array access.
generateArrayAccess :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateArrayAccess (AT.ArrayAccess loc (AT.Var _ name _) indexExpr) = do
  maybeVar <- getVar name
  ptr <- case maybeVar of
    Just arrayPtr -> return arrayPtr
    Nothing -> E.throwError $ CodegenError loc $ VariableNotFound name
  index <- generateExpr indexExpr
  elementPtr <- I.gep ptr [IC.int32 0, index]
  I.load elementPtr 0
generateArrayAccess expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for struct access.
generateStructAccess :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateStructAccess (AT.StructAccess loc (AT.StructAccess _ structExpr (AT.Var nestedStructNameLoc nestedStructName _)) (AT.Var innerFieldLoc innerFieldName innerFieldType)) = do
  case structExpr of
    AT.Var _ _ (AT.TStruct _ structFields) -> do
      structPtr <- getStructFieldPointer structExpr nestedStructName
      let nestedStructType = M.fromJust $ L.find ((== nestedStructName) . fst) structFields
      nestedStructIndex <- case nestedStructType of
        (_, AT.TStruct _ innerFields) -> return $ fromIntegral $ M.fromJust $ L.findIndex ((== innerFieldName) . fst) innerFields
        _ -> E.throwError $ CodegenError nestedStructNameLoc $ UnsupportedStructureAccess (AT.Var innerFieldLoc innerFieldName innerFieldType)
      innerFieldPtr <- I.gep structPtr [IC.int32 0, IC.int32 nestedStructIndex]
      I.load innerFieldPtr 0
    -- AT.StructAccess loc expr field -> do
    _ -> E.throwError $ CodegenError loc $ UnsupportedStructureAccess structExpr
generateStructAccess (AT.StructAccess _ structExpr (AT.Var _ fieldName _)) = do
  fieldPtr <- getStructFieldPointer structExpr fieldName
  I.load fieldPtr 0
generateStructAccess expr = E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

getStructFieldPointer :: (MonadCodegen m) => AT.Expr -> String -> m AST.Operand
getStructFieldPointer (AT.Var loc name (AT.TStruct _ fields)) fieldName = do
  maybeVar <- getVar name
  ptr <- case maybeVar of
    Just structPtr -> return structPtr
    Nothing -> E.throwError $ CodegenError loc $ VariableNotFound name
  I.gep ptr [IC.int32 0, IC.int32 (findStructFieldIndex fields fieldName)]
getStructFieldPointer expr _ = E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

findStructFieldIndex :: [(String, AT.Type)] -> String -> Integer
findStructFieldIndex fields' name' = fromIntegral $ M.fromJust $ L.findIndex ((== name') . fst) fields'

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
    _ -> E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedType typ
generateCast expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for for loops.
generateForLoop :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateForLoop (AT.For _ initExpr condExpr stepExpr bodyExpr) = mdo
  _ <- generateExpr initExpr

  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "for.cond"
  condVal <- generateExpr condExpr
  I.condBr condVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "for.body"

  oldLoopState <- S.gets loopState
  S.modify (\s -> s {loopState = Just (stepBlock, exitBlock)})

  _ <- generateExpr bodyExpr

  S.modify (\s -> s {loopState = oldLoopState})

  I.br stepBlock

  stepBlock <- IRM.block `IRM.named` U.stringToByteString "for.step"
  _ <- generateExpr stepExpr
  I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "for.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
generateForLoop expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedForDefinition expr

-- | Generate LLVM code for while loops.
generateWhileLoop :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateWhileLoop (AT.While _ condExpr bodyExpr) = mdo
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "while.cond"
  condVal <- generateExpr condExpr
  I.condBr condVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "while.body"

  oldLoopState <- S.gets loopState
  S.modify (\s -> s {loopState = Just (condBlock, exitBlock)})

  _bodyVal <- generateExpr bodyExpr

  S.modify (\s -> s {loopState = oldLoopState})

  bodyTerminated <- IRM.hasTerminator
  CM.unless bodyTerminated $
    I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "while.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
generateWhileLoop expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedWhileDefinition expr

-- | Generate LLVM code for break statements.
generateBreak :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBreak (AT.Break loc) = do
  state <- S.get
  case loopState state of
    Just (_, breakBlock) -> do
      I.br breakBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CodegenError loc BreakOutsideLoop
generateBreak expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

generateContinue :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateContinue (AT.Continue loc) = do
  state <- S.get
  case loopState state of
    Just (continueBlock, _) -> do
      I.br continueBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CodegenError loc ContinueOutsideLoop
generateContinue expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for assignments.
generateAssignment :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateAssignment (AT.Assignment _ expr valueExpr) = do
  value <- generateExpr valueExpr
  case expr of
    AT.Var _ name _ -> do
      maybeVar <- getVar name
      case maybeVar of
        Just ptr -> do
          I.store ptr 0 value
          pure value
        Nothing -> E.throwError $ CodegenError (U.getLoc expr) $ VariableNotFound name
    AT.ArrayAccess _ (AT.Var _ name _) indexExpr -> do
      maybeVar <- getVar name
      ptr <- case maybeVar of
        Just arrayPtr -> return arrayPtr
        Nothing -> E.throwError $ CodegenError (U.getLoc expr) $ VariableNotFound name
      index <- generateExpr indexExpr
      elementPtr <- I.gep ptr [IC.int32 0, index]
      I.store elementPtr 0 value
      pure value
    _ -> E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr
generateAssignment expr =
  E.throwError $ CodegenError (U.getLoc expr) $ UnsupportedDefinition expr
