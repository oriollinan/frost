{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.ExprGen where

import qualified Codegen.State as CS
import qualified LLVM.AST as AST

class ExprGen a where
  generateExpr :: (CS.MonadCodegen m) => a -> m AST.Operand
