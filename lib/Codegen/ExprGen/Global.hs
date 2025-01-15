{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module Codegen.ExprGen.Global where

import qualified Ast.Types as AT
import qualified Codegen.Errors as CC
import qualified Codegen.ExprGen.ExprGen as EG
import qualified Codegen.ExprGen.Function as EFU
import qualified Codegen.ExprGen.Types as ET
import qualified Codegen.ExprGen.Variable as EV
import qualified Codegen.State as CS
import qualified Codegen.Utils as U
import qualified Control.Monad as CM
import qualified Control.Monad.Except as E
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.IRBuilder.Module as M
import qualified Shared.Utils as SU

-- | Generate LLVM code for global expressions.
generateGlobal :: (CS.MonadCodegen m, EG.ExprGen AT.Expr) => AT.Expr -> m ()
generateGlobal expr = case expr of
  AT.Function {} -> CM.void $ EFU.generateFunction expr
  AT.ForeignFunction {} -> CM.void $ EFU.generateForeignFunction expr
  AT.Declaration {} -> CM.void $ generateGlobalDeclaration expr
  _ -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedTopLevel expr

-- | Generate LLVM code for global declarations.
generateGlobalDeclaration :: (CS.MonadCodegen m, EG.ExprGen AT.Expr) => AT.Expr -> m ()
generateGlobalDeclaration (AT.Declaration _ name typ initExpr) = do
  let varType = ET.toLLVM typ
  case initExpr of
    Just (AT.Lit loc lit) -> do
      initConstant <- EV.generateConstant lit loc
      var <- M.global (AST.Name $ U.stringToByteString name) varType initConstant
      CS.addGlobalVar name var
    Nothing -> do
      var <- M.global (AST.Name $ U.stringToByteString name) varType (C.Undef varType)
      CS.addGlobalVar name var
    Just expr -> E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedGlobalDeclaration expr
generateGlobalDeclaration expr = E.throwError $ CC.CodegenError (SU.getLoc expr) $ CC.UnsupportedGlobalDeclaration expr
