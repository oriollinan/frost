{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}

module Codegen.ExprGen.ExprGen where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import qualified Codegen.ExprGen.Types as ET -- Phone Home
import qualified Codegen.State as CS
import qualified Codegen.Utils as U
import qualified Control.Monad as CM
import qualified Control.Monad.Except as E
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

-- | Generate LLVM code for global expressions.
generateGlobal :: (CS.MonadCodegen m) => AT.Expr -> m ()
generateGlobal expr = case expr of
  AT.Function {} -> CM.void $ generateFunction expr
  AT.ForeignFunction {} -> CM.void $ generateForeignFunction expr
  _ -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedTopLevel expr

-- | Generate LLVM code for an expression.
class ExprGen a where
  generateExpr :: (CS.MonadCodegen m) => a -> m AST.Operand

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
generateConstant :: (CS.MonadCodegen m) => AT.Literal -> AT.SrcLoc -> m C.Constant
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
          _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedLiteral lit
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
generateLiteral :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateLiteral (AT.Lit loc lit) = do
  constant <- generateConstant lit loc
  pure $ AST.ConstantOperand constant
generateLiteral expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for global variables.
createGlobalString :: (CS.MonadCodegen m) => String -> m AST.Operand
createGlobalString str = do
  let strConst =
        C.Array
          (T.IntegerType 8)
          (map (C.Int 8 . fromIntegral . fromEnum) (str ++ "\0"))
  let strType = T.ArrayType (fromIntegral $ length str + 1) (T.IntegerType 8)
  name <- CS.fresh
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
generateBinaryOp :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
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
      _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
    (T.IntegerType _, T.IntegerType _) -> case findIntOperator op of
      Just f -> f v1 v2
      Nothing -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
    (T.FloatingPointType _, T.FloatingPointType _) -> case findFloatOperator op of
      Just f -> f v1 v2
      Nothing -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
    _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedOperator op
  where
    findIntOperator op' = L.find ((== op') . opMapping) integerBinaryOperators >>= Just . opFunction
    findFloatOperator op' = L.find ((== op') . opMapping) floatingPointBinaryOperators >>= Just . opFunction
generateBinaryOp expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Binary operation data type.
data BinaryOp m = BinaryOp
  { opMapping :: AT.Operation,
    opFunction :: AST.Operand -> AST.Operand -> m AST.Operand
  }

-- | List of supported integer binary operators.
integerBinaryOperators :: (CS.MonadCodegen m) => [BinaryOp m]
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
floatingPointBinaryOperators :: (CS.MonadCodegen m) => [BinaryOp m]
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
unaryOperators :: (CS.MonadCodegen m) => AT.SrcLoc -> [UnaryOp m]
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
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PreInc,
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
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PreDec,
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
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PostInc,
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
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.PostDec,
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
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.Not,
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
        _ -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator AT.BitNot,
    UnaryOp AT.Deref $ \operand -> do
      I.load operand 0,
    UnaryOp AT.AddrOf $ \operand -> do
      ptr <- I.alloca (TD.typeOf operand) Nothing 0
      I.store ptr 0 operand
      pure ptr
  ]

-- | Generate LLVM code for unary operations.
generateUnaryOp :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateUnaryOp (AT.UnaryOp loc op expr) = do
  operand <- generateExpr expr
  case findOperator op of
    Just f -> f operand
    Nothing -> E.throwError $ CC.CodegenError loc $ CC.UnsupportedUnaryOperator op
  where
    findOperator op' = L.find ((== op') . unaryMapping) (unaryOperators loc) >>= Just . unaryFunction
generateUnaryOp expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for variable references.
generateVar :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateVar (AT.Var loc name _) = do
  maybeVar <- CS.getVar name
  case maybeVar of
    Nothing ->
      E.throwError $ CC.CodegenError loc $ CC.VariableNotFound name
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
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for blocks.
generateBlock :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateBlock (AT.Block []) = pure $ AST.ConstantOperand $ C.Undef T.void
generateBlock (AT.Block exprs) = do
  last <$> traverse generateExpr exprs
generateBlock expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for `if` expressions.
generateIf :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
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
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for function definitions.
generateFunction :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunction (AT.Function _ name (AT.TFunction ret params var) paramNames body) = do
  let funcName = AST.Name $ U.stringToByteString name
      paramTypes = zipWith mkParam params paramNames
      funcType = T.ptr $ T.FunctionType (ET.toLLVM ret) (map fst paramTypes) var
  CS.addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName
  M.function funcName paramTypes (ET.toLLVM ret) $ \ops -> do
    S.modify (\s -> s {CS.localState = []})
    S.zipWithM_ CS.addVar paramNames ops
    oldAllocatedVars <- S.gets CS.allocatedVars
    preAllocateVars body
    result <- generateExpr body
    I.ret result
    S.modify (\s -> s {CS.allocatedVars = oldAllocatedVars})
  where
    mkParam t n = (ET.toLLVM t, M.ParameterName $ U.stringToByteString n)
generateFunction expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for foreign function definitions.
generateForeignFunction :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateForeignFunction (AT.ForeignFunction _ name (AT.TFunction ret params var)) = do
  let funcType = T.ptr $ T.FunctionType (ET.toLLVM ret) (map ET.toLLVM params) var
      funcName = AST.Name $ U.stringToByteString name

  _ <-
    (if var then M.externVarArgs else M.extern)
      funcName
      (filter (/= T.void) $ map ET.toLLVM params)
      (ET.toLLVM ret)

  CS.addGlobalVar name $ AST.ConstantOperand $ C.GlobalReference funcType funcName

  pure $ AST.ConstantOperand $ C.GlobalReference funcType funcName
generateForeignFunction expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for declarations.
generateDeclaration :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateDeclaration (AT.Declaration loc name typ mInitExpr) = do
  maybeVar <- CS.getVar name
  case maybeVar of
    Just ptr -> do
      case mInitExpr of
        Just initExpr -> do
          initValue <- generateExpr initExpr
          -- TODO: This makes us lose precision in some cases,
          -- e.g. when initializing a float with an integer
          initValue' <- ET.ensureMatchingType loc initValue (ET.toLLVM typ)
          I.store ptr 0 initValue'
          I.load ptr 0
        Nothing -> I.load ptr 0
    Nothing -> E.throwError $ CC.CodegenError loc $ CC.VariableNotFound name
generateDeclaration expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for return statements.
generateReturn :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
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
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for function calls.
generateFunctionCall :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateFunctionCall (AT.Call loc (AT.Var _ funcName _) args) = do
  maybeFunc <- CS.getVar funcName
  case maybeFunc of
    Just funcOperand -> do
      argOperands <- mapM generateExpr args
      I.call funcOperand (map (,[]) argOperands)
    Nothing ->
      E.throwError $ CC.CodegenError loc $ CC.UnsupportedFunctionCall funcName
generateFunctionCall expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Check the type of an argument.
checkArgumentType :: (CS.MonadCodegen m) => T.Type -> AT.Expr -> m ()
checkArgumentType expectedType expr = do
  operand <- generateExpr expr
  let actualType = TD.typeOf operand
  CM.when (actualType /= expectedType) $
    E.throwError $
      CC.CodegenError (SU.getLoc expr) $
        CC.UnsupportedFunctionCall "Argument type mismatch"

-- | Generate LLVM code for array access.
generateArrayAccess :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateArrayAccess (AT.ArrayAccess _ arrayExpr indexExpr) = do
  arrayOperand <- generateExpr arrayExpr
  indexOperand <- generateExpr indexExpr
  I.gep arrayOperand [indexOperand]
generateArrayAccess expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for struct access, recursively traversing all levels.
generateStructAccess :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateStructAccess expr = do
  (ptr, _) <- getStructFieldPointer expr
  I.load ptr 0

-- | Get the pointer to a struct field.
getStructFieldPointer :: (CS.MonadCodegen m) => AT.Expr -> m (AST.Operand, AT.Type)
getStructFieldPointer (AT.StructAccess structLoc structExpr (AT.Var _ fieldName _)) = do
  (parentPtr, parentType) <- getStructFieldPointer structExpr
  case parentType of
    AT.TStruct _ structFields -> do
      fieldIndex <- case L.findIndex ((== fieldName) . fst) structFields of
        Just index -> return $ fromIntegral index
        Nothing -> E.throwError $ CC.CodegenError structLoc $ CC.StructureFieldNotFound fieldName
      fieldPtr <- I.gep parentPtr [IC.int32 0, IC.int32 fieldIndex]
      let fieldType = snd $ structFields !! fromIntegral fieldIndex
      return (fieldPtr, fieldType)
    _ -> E.throwError $ CC.CodegenError structLoc $ CC.UnsupportedStructureAccess structExpr
getStructFieldPointer (AT.Var structLoc structName structType) = do
  maybeVar <- CS.getVar structName
  ptr <- case maybeVar of
    Just structPtr -> return structPtr
    Nothing -> E.throwError $ CC.CodegenError structLoc $ CC.VariableNotFound structName
  return (ptr, structType)
getStructFieldPointer expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedStructureAccess expr

-- | Generate LLVM code for type casts.
generateCast :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateCast (AT.Cast _ typ expr) = do
  operand <- generateExpr expr
  ET.llvmCast (SU.getLoc expr) operand (TD.typeOf operand) (ET.toLLVM typ)
generateCast expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Convert an operand to a boolean value.
toBool :: (CS.MonadCodegen m) => AT.SrcLoc -> AST.Operand -> m AST.Operand
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
      E.throwError $ CC.CodegenError loc $ CC.UnsupportedType AT.TVoid

-- | Pre-allocate variables before generating code.
preAllocateVars :: (CS.MonadCodegen m) => AT.Expr -> m ()
preAllocateVars (AT.Assignment _ (AT.Var _ name typ) _) = do
  let llvmType = ET.toLLVM typ
  existingVar <- CS.getVar name
  CM.when (M.isNothing existingVar) $ do
    ptr <- I.alloca llvmType Nothing 0
    S.modify (\s -> s {CS.allocatedVars = (name, ptr) : CS.allocatedVars s})
preAllocateVars (AT.Declaration _ name typ init') = do
  let llvmType = ET.toLLVM typ
  existingVar <- CS.getVar name
  CM.when (M.isNothing existingVar) $ do
    ptr <- I.alloca llvmType Nothing 0
    S.modify (\s -> s {CS.allocatedVars = (name, ptr) : CS.allocatedVars s})
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
generateForLoop :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateForLoop (AT.For loc initExpr condExpr stepExpr bodyExpr) = mdo
  _ <- generateExpr initExpr
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "for.cond"
  condVal <- generateExpr condExpr
  boolCondVal <- toBool loc condVal
  I.condBr boolCondVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "for.body"
  oldLoopState <- S.gets CS.loopState
  S.modify (\s -> s {CS.loopState = Just (stepBlock, exitBlock)})

  _ <- generateExpr bodyExpr

  S.modify (\s -> s {CS.loopState = oldLoopState})
  I.br stepBlock

  stepBlock <- IRM.block `IRM.named` U.stringToByteString "for.step"
  _ <- generateExpr stepExpr
  I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "for.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
generateForLoop expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedForDefinition expr

-- | Generate LLVM code for while loops.
generateWhileLoop :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateWhileLoop (AT.While loc condExpr bodyExpr) = mdo
  I.br condBlock

  condBlock <- IRM.block `IRM.named` U.stringToByteString "while.cond"
  condVal <- generateExpr condExpr
  boolCondVal <- toBool loc condVal
  I.condBr boolCondVal bodyBlock exitBlock

  bodyBlock <- IRM.block `IRM.named` U.stringToByteString "while.body"
  oldLoopState <- S.gets CS.loopState
  S.modify (\s -> s {CS.loopState = Just (condBlock, exitBlock)})

  _ <- generateExpr bodyExpr

  S.modify (\s -> s {CS.loopState = oldLoopState})
  bodyTerminated <- IRM.hasTerminator
  CM.unless bodyTerminated $ I.br condBlock

  exitBlock <- IRM.block `IRM.named` U.stringToByteString "while.exit"
  pure $ AST.ConstantOperand $ C.Null T.i8
generateWhileLoop expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedWhileDefinition expr

-- | Generate LLVM code for break statements.
generateBreak :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateBreak (AT.Break loc) = do
  state <- S.get
  case CS.loopState state of
    Just (_, breakBlock) -> do
      I.br breakBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CC.CodegenError loc CC.BreakOutsideLoop
generateBreak expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for continue statements.
generateContinue :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateContinue (AT.Continue loc) = do
  state <- S.get
  case CS.loopState state of
    Just (continueBlock, _) -> do
      I.br continueBlock
      pure $ AST.ConstantOperand $ C.Undef T.void
    Nothing -> E.throwError $ CC.CodegenError loc CC.ContinueOutsideLoop
generateContinue expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for assignments.
generateAssignment :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
generateAssignment (AT.Assignment _ expr valueExpr) = do
  value <- generateExpr valueExpr
  case expr of
    AT.Var _ name _ -> do
      maybeVar <- CS.getVar name
      case maybeVar of
        Just ptr -> do
          I.store ptr 0 value
          pure value
        Nothing -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.VariableNotFound name
    AT.ArrayAccess _ (AT.Var _ name _) indexExpr -> do
      maybeVar <- CS.getVar name
      ptr <- case maybeVar of
        Just arrayPtr -> return arrayPtr
        Nothing -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.VariableNotFound name
      index <- generateExpr indexExpr
      elementPtr <- I.gep ptr [IC.int32 0, index]
      I.store elementPtr 0 value
      pure value
    AT.UnaryOp _ AT.Deref (AT.Var _ name _) -> do
      maybeVar <- CS.getVar name
      case maybeVar of
        Just ptr -> do
          actualPtr <- I.load ptr 0
          I.store actualPtr 0 value
          pure value
        Nothing -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.VariableNotFound name
    AT.StructAccess {} -> do
      (fieldPtr, _) <- getStructFieldPointer expr
      I.store fieldPtr 0 value
      pure value
    _ -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr
generateAssignment expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr
