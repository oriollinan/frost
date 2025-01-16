{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Codegen.ExprGen.Function where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import {-# SOURCE #-} Codegen.ExprGen.ExprGen (ExprGen (..))
import qualified Codegen.ExprGen.Types as ET
import qualified Codegen.State as CS
import qualified Codegen.Utils as U
import qualified Control.Monad as CM
import qualified Control.Monad.Except as E
import qualified Control.Monad.State as S
import qualified Data.Foldable as FD
import qualified Data.Maybe as M
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Type as T
import qualified LLVM.IRBuilder.Instruction as I
import qualified LLVM.IRBuilder.Module as M
import qualified Shared.Utils as SU

-- | Generate LLVM code for function definitions.
generateFunction :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
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
    case ret of
      AT.TVoid -> I.retVoid
      _ -> I.ret result
    S.modify (\s -> s {CS.allocatedVars = oldAllocatedVars})
  where
    mkParam (AT.TFunction retType paramTypes isVar) n =
      ( T.ptr $
          T.FunctionType
            (ET.toLLVM retType)
            (map ET.toLLVM paramTypes)
            isVar,
        M.ParameterName $ U.stringToByteString n
      )
    mkParam t n = (ET.toLLVM t, M.ParameterName $ U.stringToByteString n)
generateFunction expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Pre-allocate variables before generating code.
preAllocateVars :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m ()
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
preAllocateVars (AT.From _ startExpr endExpr stepExpr varExpr bodyExpr) = do
  preAllocateVars startExpr
  preAllocateVars endExpr
  preAllocateVars stepExpr
  preAllocateVars varExpr
  preAllocateVars bodyExpr
preAllocateVars (AT.While _ condExpr bodyExpr) = do
  preAllocateVars condExpr
  preAllocateVars bodyExpr
preAllocateVars (AT.Break _) = pure ()
preAllocateVars (AT.Continue _) = pure ()
preAllocateVars (AT.Assignment _ _ valueExpr) = preAllocateVars valueExpr
preAllocateVars (AT.Function _ _ _ _ bodyExpr) = preAllocateVars bodyExpr
preAllocateVars _ = pure ()

-- | Generate LLVM code for foreign function definitions.
generateForeignFunction :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
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

-- | Generate LLVM code for function calls.
generateFunctionCall :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
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
