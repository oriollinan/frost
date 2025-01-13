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
import qualified Data.Foldable as FD
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as FF
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Linkage as LK
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified LLVM.AST.Visibility as V
import qualified LLVM.IRBuilder.Constant as IC
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM
import qualified Shared.Utils as SU

-- | Type alias for the local code generation state.
type LocalState = [(String, AST.Operand)]

-- | Type alias for the global code generation state.
type GlobalState = [(String, AST.Operand)]

-- | Type alias for the loop code generation state.
type LoopState = Maybe (AST.Name, AST.Name)

-- | Type alias for the variables name .
type UniqueNameState = Integer

-- | Combined state for code generation.
data CodegenState = CodegenState
  { localState :: LocalState,
    globalState :: GlobalState,
    loopState :: LoopState,
    allocatedVars :: LocalState,
    uniqueNameState :: UniqueNameState
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
data CodegenError = CodegenError
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
    return $
      lookup name (allocatedVars state)
        `S.mplus` lookup name (localState state)
        `S.mplus` lookup name (globalState state)
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
    AT.TUnknown -> T.void

-- | Generate LLVM code for a program.
codegen :: AT.Program -> Either CodegenError AST.Module
codegen program =
  E.runExcept $
    M.buildModuleT (U.stringToByteString $ AT.sourceFile program) $
      IRM.runIRBuilderT IRM.emptyIRBuilder $
        S.evalStateT (mapM_ (generateGlobal . snd) (AT.globals program)) (CodegenState [] [] Nothing [] 0)

-- | Generate LLVM code for global expressions.
generateGlobal :: (MonadCodegen m) => AT.Expr -> m ()
generateGlobal expr = case expr of
  AT.Function {} -> CM.void $ generateFunction expr
  AT.ForeignFunction {} -> CM.void $ generateForeignFunction expr
  _ -> E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedTopLevel expr

-- Generates a fresh unique name.
fresh :: (MonadCodegen m) => m AST.Name
fresh = do
  state <- S.get
  let uniqueName = uniqueNameState state
  S.put $ state {uniqueNameState = uniqueName + 1}
  let fullName = "_" ++ show uniqueName
  return $ AST.Name (U.stringToByteString fullName)

-- Generates a fresh unique name with the given prefix.
freshName :: (MonadCodegen m) => String -> m AST.Name
freshName prefix = do
  state <- S.get
  let uniqueName = uniqueNameState state
  S.put $ state {uniqueNameState = uniqueName + 1}
  let fullName = prefix ++ show uniqueName
  return $ AST.Name (U.stringToByteString fullName)

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
generateConstant :: (MonadCodegen m) => AT.Literal -> AT.SrcLoc -> m C.Constant
generateConstant lit loc = case lit of
  AT.LInt n -> return $ C.Int 32 (fromIntegral n)
  AT.LChar c -> return $ C.Int 8 (fromIntegral $ fromEnum c)
  AT.LBool b -> return $ C.Int 1 (if b then 1 else 0)
  AT.LNull -> return $ C.Null T.i8
  AT.LFloat f -> pure $ C.Float (FF.Double (realToFrac f))
  AT.LArray elems -> do
    let (headElem, _) = M.fromJust $ L.uncons elems
    case headElem of
      AT.LChar _ -> do
        strPtr <- createGlobalString [c | AT.LChar c <- elems]
        case strPtr of
          AST.ConstantOperand c -> return c
          _ -> E.throwError $ CodegenError loc $ UnsupportedLiteral lit
      _ -> do
        constants <- mapM (`generateConstant` loc) elems
        return $ C.Array (TD.typeOf $ head constants) constants
  AT.LStruct fields -> do
    -- We do not need the names of the fields
    -- as we only use them when accessing the fields of the struct
    let (_, values) = unzip fields
    constants <- mapM (`generateConstant` loc) values
    return $ C.Struct Nothing False constants

-- | Generate LLVM code for literals.
generateLiteral :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateLiteral (AT.Lit loc lit) = do
  constant <- generateConstant lit loc
  pure $ AST.ConstantOperand constant
generateLiteral expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for global variables.
createGlobalString :: (MonadCodegen m) => String -> m AST.Operand
createGlobalString str = do
  let strConst =
        C.Array
          (T.IntegerType 8)
          (map (C.Int 8 . fromIntegral . fromEnum) (str ++ "\0"))
  let strType = T.ArrayType (fromIntegral $ length str + 1) (T.IntegerType 8)
  name <- fresh
  let global =
        AST.GlobalVariable
          { G.name = name,
            G.linkage = LK.Private,
            G.visibility = V.Default,
            G.dllStorageClass = Nothing,
            G.threadLocalMode = Nothing,
            G.unnamedAddr = Just AST.GlobalAddr,
            G.isConstant = True,
            G.type' = strType,
            G.addrSpace = AS.AddrSpace 0,
            G.initializer = Just strConst,
            G.section = Nothing,
            G.comdat = Nothing,
            G.alignment = 1,
            G.metadata = []
          }
  M.emitDefn $ AST.GlobalDefinition global
  return $
    AST.ConstantOperand $
      C.GetElementPtr
        True
        (C.GlobalReference (T.ptr strType) name)
        [C.Int 64 0, C.Int 64 0]

-- | Generate LLVM code for binary operations.
generateBinaryOp :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBinaryOp (AT.Op loc op e1 e2) = do
  v1 <- generateExpr e1
  v2 <- generateExpr e2
  let ty1 = TD.typeOf v1
      ty2 = TD.typeOf v2
  case (ty1, ty2) of
    (T.PointerType _ _, T.IntegerType _) -> case op of
      AT.Add -> I.gep v1 [v2]
      AT.Sub -> do
        negV2 <- I.sub (AST.ConstantOperand $ C.Int 32 0) v2
        I.gep v1 [negV2]
      _ -> E.throwError $ CodegenError loc $ UnsupportedOperator op
    (T.IntegerType _, T.IntegerType _) -> case findIntOperator op of
      Just f -> f v1 v2
      Nothing -> E.throwError $ CodegenError loc $ UnsupportedOperator op
    (T.FloatingPointType _, T.FloatingPointType _) -> case findFloatOperator op of
      Just f -> f v1 v2
      Nothing -> E.throwError $ CodegenError loc $ UnsupportedOperator op
    _ -> E.throwError $ CodegenError loc $ UnsupportedOperator op
  where
    findIntOperator op' = L.find ((== op') . opMapping) integerBinaryOperators >>= Just . opFunction
    findFloatOperator op' = L.find ((== op') . opMapping) floatingPointBinaryOperators >>= Just . opFunction
generateBinaryOp expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Binary operation data type.
data BinaryOp m = BinaryOp
  { opMapping :: AT.Operation,
    opFunction :: AST.Operand -> AST.Operand -> m AST.Operand
  }

-- | List of supported integer binary operators.
integerBinaryOperators :: (MonadCodegen m) => [BinaryOp m]
integerBinaryOperators =
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

-- | List of supported floating-point binary operators.
floatingPointBinaryOperators :: (MonadCodegen m) => [BinaryOp m]
floatingPointBinaryOperators =
  [ BinaryOp AT.Add I.fadd,
    BinaryOp AT.Sub I.fsub,
    BinaryOp AT.Mul I.fmul,
    BinaryOp AT.Div I.fdiv,
    BinaryOp AT.Mod I.frem
  ]
    ++ map mkComparisonOp comparisonOps
  where
    mkComparisonOp (op, pre) = BinaryOp op (I.fcmp pre)
    comparisonOps =
      [ (AT.Lt, FP.OLT),
        (AT.Gt, FP.OGT),
        (AT.Lte, FP.OLE),
        (AT.Gte, FP.OGE),
        (AT.Eq, FP.OEQ),
        (AT.Ne, FP.ONE)
      ]

-- | Unary operation data type.
data UnaryOp m = UnaryOp
  { unaryMapping :: AT.UnaryOperation,
    unaryFunction :: AST.Operand -> m AST.Operand
  }

-- | List of supported unary operators.
unaryOperators :: (MonadCodegen m) => AT.SrcLoc -> [UnaryOp m]
unaryOperators loc =
  [ UnaryOp AT.PreInc $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          val <- I.load operand 0
          newVal <- I.add val (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure newVal
        T.IntegerType bits -> do
          I.add operand (AST.ConstantOperand $ C.Int bits 1)
        T.FloatingPointType T.FloatFP -> do
          I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
        T.FloatingPointType T.DoubleFP -> do
          I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
        _ -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator AT.PreInc,
    UnaryOp AT.PreDec $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          val <- I.load operand 0
          newVal <- I.sub val (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure newVal
        T.IntegerType bits -> do
          I.sub operand (AST.ConstantOperand $ C.Int bits 1)
        T.FloatingPointType T.FloatFP -> do
          I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
        T.FloatingPointType T.DoubleFP -> do
          I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
        _ -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator AT.PreDec,
    UnaryOp AT.PostInc $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          oldVal <- I.load operand 0
          newVal <- I.add oldVal (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure oldVal
        T.IntegerType bits -> do
          let oldVal = operand
          _ <- I.add operand (AST.ConstantOperand $ C.Int bits 1)
          pure oldVal
        T.FloatingPointType T.FloatFP -> do
          let oldVal = operand
          _ <- I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
          pure oldVal
        T.FloatingPointType T.DoubleFP -> do
          let oldVal = operand
          _ <- I.fadd operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
          pure oldVal
        _ -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator AT.PostInc,
    UnaryOp AT.PostDec $ \operand -> do
      case TD.typeOf operand of
        T.PointerType _ _ -> do
          oldVal <- I.load operand 0
          newVal <- I.sub oldVal (AST.ConstantOperand $ C.Int 32 1)
          I.store operand 0 newVal
          pure oldVal
        T.IntegerType bits -> do
          let oldVal = operand
          _ <- I.sub operand (AST.ConstantOperand $ C.Int bits 1)
          pure oldVal
        T.FloatingPointType T.FloatFP -> do
          let oldVal = operand
          _ <- I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Single 1.0)
          pure oldVal
        T.FloatingPointType T.DoubleFP -> do
          let oldVal = operand
          _ <- I.fsub operand (AST.ConstantOperand $ C.Float $ FF.Double 1.0)
          pure oldVal
        _ -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator AT.PostDec,
    UnaryOp AT.Not $ \operand -> do
      let operandType = TD.typeOf operand
      case operandType of
        T.PointerType _ _ -> do
          I.icmp IP.EQ operand (AST.ConstantOperand $ C.Null operandType)
        T.IntegerType _ -> I.xor operand (AST.ConstantOperand $ C.Int 32 (-1))
        T.FloatingPointType T.FloatFP -> do
          I.fcmp FP.OEQ operand (AST.ConstantOperand $ C.Float $ FF.Single 0.0)
        T.FloatingPointType T.DoubleFP -> do
          I.fcmp FP.OEQ operand (AST.ConstantOperand $ C.Float $ FF.Double 0.0)
        _ -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator AT.Not,
    UnaryOp AT.BitNot $ \operand -> do
      case TD.typeOf operand of
        T.IntegerType bits ->
          I.xor operand (AST.ConstantOperand $ C.Int bits (-1))
        T.FloatingPointType T.FloatFP -> do
          intValue <- I.bitcast operand (T.IntegerType 32)
          notted <- I.xor intValue (AST.ConstantOperand $ C.Int 32 (-1))
          I.bitcast notted (T.FloatingPointType T.FloatFP)
        T.FloatingPointType T.DoubleFP -> do
          intValue <- I.bitcast operand (T.IntegerType 64)
          notted <- I.xor intValue (AST.ConstantOperand $ C.Int 64 (-1))
          I.bitcast notted (T.FloatingPointType T.DoubleFP)
        _ -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator AT.BitNot,
    UnaryOp AT.Deref $ \operand -> do
      I.load operand 0,
    UnaryOp AT.AddrOf $ \operand -> do
      ptr <- I.alloca (TD.typeOf operand) Nothing 0
      I.store ptr 0 operand
      pure ptr
  ]

-- | Generate LLVM code for unary operations.
generateUnaryOp :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateUnaryOp (AT.UnaryOp loc op expr) = do
  operand <- generateExpr expr
  case findOperator op of
    Just f -> f operand
    Nothing -> E.throwError $ CodegenError loc $ UnsupportedUnaryOperator op
  where
    findOperator op' = L.find ((== op') . unaryMapping) (unaryOperators loc) >>= Just . unaryFunction
generateUnaryOp expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for variable references.
generateVar :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateVar (AT.Var loc name _) = do
  maybeVar <- getVar name
  case maybeVar of
    Nothing ->
      E.throwError $ CodegenError loc $ VariableNotFound name
    Just ptr -> do
      let varTy = TD.typeOf ptr
      case varTy of
        T.PointerType (T.FunctionType {}) _ ->
          return ptr
        T.PointerType (T.IntegerType 8) _ ->
          return ptr
        T.PointerType _ _ ->
          I.load ptr 0
        _ ->
          return ptr
generateVar expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for blocks.
generateBlock :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateBlock (AT.Block []) = pure $ AST.ConstantOperand $ C.Undef T.void
generateBlock (AT.Block exprs) = do
  last <$> traverse generateExpr exprs
generateBlock expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

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
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for function definitions.
generateFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunction (AT.Function _ name (AT.TFunction ret params var) paramNames body) = do
  let funcName = AST.Name $ U.stringToByteString name
      paramTypes = zipWith mkParam params paramNames
      funcType = T.ptr $ T.FunctionType (toLLVM ret) (map fst paramTypes) var
  addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName
  M.function funcName paramTypes (toLLVM ret) $ \ops -> do
    S.modify (\s -> s {localState = []})
    S.zipWithM_ addVar paramNames ops
    oldAllocatedVars <- S.gets allocatedVars
    preAllocateVars body
    result <- generateExpr body
    I.ret result
    S.modify (\s -> s {allocatedVars = oldAllocatedVars})
  where
    mkParam t n = (toLLVM t, M.ParameterName $ U.stringToByteString n)
generateFunction expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for foreign function definitions.
generateForeignFunction :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateForeignFunction (AT.ForeignFunction _ name (AT.TFunction ret params var)) = do
  let funcType = T.ptr $ T.FunctionType (toLLVM ret) (map toLLVM params) var
      funcName = AST.Name $ U.stringToByteString name

  _ <-
    (if var then M.externVarArgs else M.extern)
      funcName
      (map toLLVM params)
      (toLLVM ret)

  addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName

  pure $ AST.ConstantOperand $ C.GlobalReference funcType funcName
generateForeignFunction expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Convert an operand to match a desired LLVM type if needed.
ensureMatchingType :: (MonadCodegen m) => AT.SrcLoc -> AST.Operand -> T.Type -> m AST.Operand
ensureMatchingType loc val targetTy
  | TD.typeOf val == targetTy = pure val
  | otherwise = case (TD.typeOf val, targetTy) of
      (T.IntegerType fromW, T.IntegerType toW)
        | fromW < toW -> I.zext val targetTy
        | fromW > toW -> I.trunc val targetTy
      (T.FloatingPointType _, T.FloatingPointType _) -> I.fptrunc val targetTy
      (T.IntegerType _, T.FloatingPointType _) -> I.sitofp val targetTy
      (T.FloatingPointType _, T.IntegerType _) -> I.fptosi val targetTy
      (T.PointerType _ _, T.PointerType _ _) -> I.bitcast val targetTy
      (T.IntegerType _, T.PointerType _ _) -> I.inttoptr val targetTy
      (T.PointerType _ _, T.IntegerType _) -> I.ptrtoint val targetTy
      (T.ArrayType _ _, T.PointerType _ _) -> I.bitcast val targetTy
      (T.ArrayType _ _, T.ArrayType _ _) -> I.bitcast val targetTy
      _ -> E.throwError $ CodegenError loc $ UnsupportedType AT.TVoid

-- | Generate LLVM code for declarations.
generateDeclaration :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateDeclaration (AT.Declaration loc name typ mInitExpr) = do
  maybeVar <- getVar name
  case maybeVar of
    Just ptr -> do
      case mInitExpr of
        Just initExpr -> do
          initValue <- generateExpr initExpr
          -- TODO: This makes us lose precision in some cases,
          -- e.g. when initializing a float with an integer
          initValue' <- ensureMatchingType loc initValue (toLLVM typ)
          I.store ptr 0 initValue'
          I.load ptr 0
        Nothing -> I.load ptr 0
    Nothing -> E.throwError $ CodegenError loc $ VariableNotFound name
generateDeclaration expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

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
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for function calls.
generateFunctionCall :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunctionCall (AT.Call loc (AT.Var _ funcName _) args) = do
  maybeFunc <- getVar funcName
  case maybeFunc of
    Just funcOperand -> do
      argOperands <- mapM generateExpr args
      I.call funcOperand (map (,[]) argOperands)
    Nothing ->
      E.throwError $ CodegenError loc $ UnsupportedFunctionCall funcName
generateFunctionCall expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Check the type of an argument.
checkArgumentType :: (MonadCodegen m) => T.Type -> AT.Expr -> m ()
checkArgumentType expectedType expr = do
  operand <- generateExpr expr
  let actualType = TD.typeOf operand
  CM.when (actualType /= expectedType) $
    E.throwError $
      CodegenError (SU.getLoc expr) $
        UnsupportedFunctionCall "Argument type mismatch"

-- | Generate LLVM code for array access.
generateArrayAccess :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateArrayAccess (AT.ArrayAccess _ arrayExpr indexExpr) = do
  arrayOperand <- generateExpr arrayExpr
  indexOperand <- generateExpr indexExpr
  I.gep arrayOperand [indexOperand]
generateArrayAccess expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

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
    _ -> E.throwError $ CodegenError loc $ UnsupportedStructureAccess structExpr
generateStructAccess (AT.StructAccess _ structExpr (AT.Var _ fieldName _)) = do
  fieldPtr <- getStructFieldPointer structExpr fieldName
  I.load fieldPtr 0
generateStructAccess expr = E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

getStructFieldPointer :: (MonadCodegen m) => AT.Expr -> String -> m AST.Operand
getStructFieldPointer (AT.Var loc name (AT.TStruct _ fields)) fieldName = do
  maybeVar <- getVar name
  ptr <- case maybeVar of
    Just structPtr -> return structPtr
    Nothing -> E.throwError $ CodegenError loc $ VariableNotFound name
  I.gep ptr [IC.int32 0, IC.int32 (findStructFieldIndex fields fieldName)]
getStructFieldPointer expr _ = E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

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
      | fromBits > toBits -> I.trunc operand toType
    (T.FloatingPointType _, T.FloatingPointType _) ->
      I.fptrunc operand toType
    (T.IntegerType _, T.FloatingPointType _) ->
      I.sitofp operand toType
    (T.FloatingPointType _, T.IntegerType _) ->
      I.fptosi operand toType
    (T.PointerType _ _, T.PointerType _ _) ->
      I.bitcast operand toType
    (T.IntegerType _, T.PointerType _ _) ->
      I.inttoptr operand toType
    (T.PointerType _ _, T.IntegerType _) ->
      I.ptrtoint operand toType
    (T.ArrayType _ _, T.PointerType _ _) ->
      I.bitcast operand toType
    (T.ArrayType _ _, T.ArrayType _ _) ->
      I.bitcast operand toType
    _ ->
      E.throwError $
        CodegenError (SU.getLoc expr) $
          UnsupportedType typ
generateCast expr =
  E.throwError $
    CodegenError (SU.getLoc expr) $
      UnsupportedDefinition expr

-- | Convert an operand to a boolean value.
toBool :: (MonadCodegen m) => AT.SrcLoc -> AST.Operand -> m AST.Operand
toBool loc val = do
  let ty = TD.typeOf val
  case ty of
    T.IntegerType 1 -> pure val
    T.IntegerType 8 -> do
      let zero8 = AST.ConstantOperand (C.Int 8 0)
      I.icmp IP.NE val zero8
    T.IntegerType 32 -> do
      let zero32 = AST.ConstantOperand (C.Int 32 0)
      I.icmp IP.NE val zero32
    _ ->
      E.throwError $ CodegenError loc $ UnsupportedType AT.TVoid

-- | Pre-allocate variables before generating code.
preAllocateVars :: (MonadCodegen m) => AT.Expr -> m ()
preAllocateVars (AT.Assignment _ (AT.Var _ name typ) _) = do
  let llvmType = toLLVM typ
  existingVar <- getVar name
  CM.when (M.isNothing existingVar) $ do
    ptr <- I.alloca llvmType Nothing 0
    S.modify (\s -> s {allocatedVars = (name, ptr) : allocatedVars s})
preAllocateVars (AT.Declaration _ name typ init') = do
  let llvmType = toLLVM typ
  existingVar <- getVar name
  CM.when (M.isNothing existingVar) $ do
    ptr <- I.alloca llvmType Nothing 0
    S.modify (\s -> s {allocatedVars = (name, ptr) : allocatedVars s})
    FD.for_ init' preAllocateVars
preAllocateVars (AT.Block exprs) = mapM_ preAllocateVars exprs
preAllocateVars (AT.If _ cond thenExpr elseExpr) = do
  preAllocateVars cond
  preAllocateVars thenExpr
  maybe (return ()) preAllocateVars elseExpr
preAllocateVars (AT.For _ initExpr condExpr stepExpr bodyExpr) = do
  preAllocateVars initExpr
  preAllocateVars condExpr
  preAllocateVars stepExpr
  preAllocateVars bodyExpr
preAllocateVars (AT.While _ condExpr bodyExpr) = do
  preAllocateVars condExpr
  preAllocateVars bodyExpr
preAllocateVars (AT.Break _) = pure ()
preAllocateVars (AT.Continue _) = pure ()
preAllocateVars (AT.Assignment _ _ valueExpr) = preAllocateVars valueExpr
preAllocateVars (AT.Function _ _ _ _ bodyExpr) = preAllocateVars bodyExpr
preAllocateVars _ = pure ()

-- | Generate LLVM code for for loops.
generateForLoop :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateForLoop (AT.For loc initExpr condExpr stepExpr bodyExpr) = mdo
  condPtr <- I.alloca T.i1 Nothing 0
  _ <- generateExpr initExpr
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "for.cond"
  condVal <- generateExpr condExpr
  I.store condPtr 0 condVal
  loadedCond <- I.load condPtr 0
  boolCondVal <- toBool loc loadedCond
  I.condBr boolCondVal bodyBlock exitBlock

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
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedForDefinition expr

-- | Generate LLVM code for while loops.
generateWhileLoop :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateWhileLoop (AT.While loc condExpr bodyExpr) = mdo
  condPtr <- I.alloca T.i1 Nothing 0
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "while.cond"
  condVal <- generateExpr condExpr
  I.store condPtr 0 condVal
  loadedCond <- I.load condPtr 0
  boolCondVal <- toBool loc loadedCond
  I.condBr boolCondVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "while.body"
  oldLoopState <- S.gets loopState
  S.modify (\s -> s {loopState = Just (condBlock, exitBlock)})

  _ <- generateExpr bodyExpr

  S.modify (\s -> s {loopState = oldLoopState})
  bodyTerminated <- IRM.hasTerminator
  CM.unless bodyTerminated $ I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "while.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
generateWhileLoop expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedWhileDefinition expr

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
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

-- | Generate LLVM code for continue statements.
generateContinue :: (MonadCodegen m) => AT.Expr -> m AST.Operand
generateContinue (AT.Continue loc) = do
  state <- S.get
  case loopState state of
    Just (continueBlock, _) -> do
      I.br continueBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CodegenError loc ContinueOutsideLoop
generateContinue expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr

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
        Nothing -> E.throwError $ CodegenError (SU.getLoc expr) $ VariableNotFound name
    AT.ArrayAccess _ (AT.Var _ name _) indexExpr -> do
      maybeVar <- getVar name
      ptr <- case maybeVar of
        Just arrayPtr -> return arrayPtr
        Nothing -> E.throwError $ CodegenError (SU.getLoc expr) $ VariableNotFound name
      index <- generateExpr indexExpr
      elementPtr <- I.gep ptr [IC.int32 0, index]
      I.store elementPtr 0 value
      pure value
    AT.UnaryOp _ AT.Deref (AT.Var _ name _) -> do
      maybeVar <- getVar name
      case maybeVar of
        Just ptr -> do
          actualPtr <- I.load ptr 0
          I.store actualPtr 0 value
          pure value
        Nothing -> E.throwError $ CodegenError (SU.getLoc expr) $ VariableNotFound name
    _ -> E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr
generateAssignment expr =
  E.throwError $ CodegenError (SU.getLoc expr) $ UnsupportedDefinition expr
