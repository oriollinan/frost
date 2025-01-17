{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.Variable where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import qualified Codegen.ExprGen.Cast as EC
import qualified Codegen.ExprGen.DataValue as ED
import {-# SOURCE #-} Codegen.ExprGen.ExprGen (ExprGen (..))
import qualified Codegen.ExprGen.Types as ET
import qualified Codegen.State as CS
import qualified Control.Monad.Except as E
import qualified Data.List as L
import qualified Data.Maybe as M
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AS
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as FF
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.Linkage as LK
import qualified LLVM.AST.Type as T
import qualified LLVM.AST.Typed as TD
import qualified LLVM.AST.Visibility as V
import qualified LLVM.IRBuilder.Constant as IC
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified Shared.Utils as SU

-- | Generate LLVM code for declarations.
generateDeclaration :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateDeclaration (AT.Declaration loc name typ mInitExpr) = do
  maybeVar <- CS.getVar name
  case maybeVar of
    Just ptr -> do
      case mInitExpr of
        Just initExpr -> do
          initValue <- generateExpr initExpr
          -- TODO: This makes us lose precision in some cases,
          -- e.g. when initializing a float with an integer
          initValue' <- EC.ensureMatchingType loc initValue (ET.toLLVM typ)
          I.store ptr 0 initValue'
          I.load ptr 0
        Nothing -> I.load ptr 0
    Nothing -> E.throwError $ CC.CodegenError loc $ CC.VariableNotFound name
generateDeclaration expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for literals.
generateLiteral :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateLiteral (AT.Lit loc lit) = do
  constant <- generateConstant lit loc
  pure $ AST.ConstantOperand constant
generateLiteral expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for constants.
generateConstant :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Literal -> AT.SrcLoc -> m C.Constant
generateConstant lit loc = case lit of
  AT.LInt n -> return $ C.Int 32 (fromIntegral n)
  AT.LChar c -> return $ C.Int 8 (fromIntegral $ fromEnum c)
  AT.LBool b -> return $ C.Int 1 (if b then 1 else 0)
  AT.LNull -> return $ C.Null T.i8
  AT.LFloat f -> pure $ C.Float (FF.Single (realToFrac f))
  AT.LDouble f -> pure $ C.Float (FF.Double (realToFrac f))
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
    let (_, values) = unzip fields
    constants <- mapM (`generateConstant` loc) values
    return $ C.Struct Nothing False constants

-- | Generate LLVM code for global variables.
createGlobalString :: (CS.MonadCodegen m, ExprGen AT.Expr) => String -> m AST.Operand
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

-- | Generate LLVM code for variable references.
generateVar :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateVar (AT.Var loc name type') = do
  maybeVar <- CS.getVar name
  case maybeVar of
    Nothing ->
      E.throwError $ CC.CodegenError loc $ CC.VariableNotFound name
    Just ptr -> do
      let varTy = TD.typeOf ptr
          expectedType = ET.toLLVM type'
      if varTy == expectedType
        then return ptr
        else case varTy of
          T.PointerType (T.FunctionType {}) _ -> return ptr
          T.PointerType _ _ -> I.load ptr 0
          _ -> return ptr
generateVar expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for assignments.
generateAssignment :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
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
      (fieldPtr, _) <- ED.getStructFieldPointer expr
      I.store fieldPtr 0 value
      pure value
    _ -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr
generateAssignment expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr
