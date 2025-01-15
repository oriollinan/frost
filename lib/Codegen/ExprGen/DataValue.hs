{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.DataValue where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import {-# SOURCE #-} Codegen.ExprGen.ExprGen (ExprGen (..))
import qualified Codegen.State as CS
import qualified Control.Monad.Except as E
import qualified Data.List as L
import qualified LLVM.AST as AST
import qualified LLVM.IRBuilder.Constant as IC
import qualified LLVM.IRBuilder.Instruction as I
import qualified Shared.Utils as SU

-- | Generate LLVM code for array access.
generateArrayAccess :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateArrayAccess (AT.ArrayAccess _ arrayExpr indexExpr) = do
  arrayOperand <- generateExpr arrayExpr
  indexOperand <- generateExpr indexExpr
  I.gep arrayOperand [indexOperand]
generateArrayAccess expr =
  E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedDefinition expr

-- | Generate LLVM code for struct access, recursively traversing all levels.
generateStructAccess :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m AST.Operand
generateStructAccess expr = do
  (ptr, _) <- getStructFieldPointer expr
  I.load ptr 0

-- | Get a pointer to a struct field.
getStructFieldPointer :: (CS.MonadCodegen m, ExprGen AT.Expr) => AT.Expr -> m (AST.Operand, AT.Type)
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
