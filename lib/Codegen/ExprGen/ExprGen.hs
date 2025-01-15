{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Codegen.ExprGen.ExprGen where

import qualified Ast.Types as AT
import qualified Codegen.ExprGen.Assembly as EA
import qualified Codegen.ExprGen.Cast as EC
import qualified Codegen.ExprGen.ControlFlow as EF
import qualified Codegen.ExprGen.DataValue as ED
import qualified Codegen.ExprGen.Function as EFU
import qualified Codegen.ExprGen.Operator as EO
import qualified Codegen.ExprGen.Variable as EV
import qualified Codegen.State as CS
import qualified LLVM.AST as AST

-- | Generate LLVM code for an expression.
class ExprGen a where
  generateExpr :: (CS.MonadCodegen m) => a -> m AST.Operand

instance ExprGen AT.Expr where
  generateExpr :: (CS.MonadCodegen m) => AT.Expr -> m AST.Operand
  generateExpr expr = case expr of
    AT.Lit {} -> EV.generateLiteral expr
    AT.Var {} -> EV.generateVar expr
    AT.Function {} -> EFU.generateFunction expr
    AT.ForeignFunction {} -> EFU.generateForeignFunction expr
    AT.Declaration {} -> EV.generateDeclaration expr
    AT.If {} -> EF.generateIf expr
    AT.Block {} -> EF.generateBlock expr
    AT.Return {} -> EF.generateReturn expr
    AT.Op {} -> EO.generateBinaryOp expr
    AT.UnaryOp {} -> EO.generateUnaryOp expr
    AT.Call {} -> EFU.generateFunctionCall expr
    AT.ArrayAccess {} -> ED.generateArrayAccess expr
    AT.StructAccess {} -> ED.generateStructAccess expr
    AT.Cast {} -> EC.generateCast expr
    AT.For {} -> EF.generateForLoop expr
    AT.While {} -> EF.generateWhileLoop expr
    AT.Break {} -> EF.generateBreak expr
    AT.Continue {} -> EF.generateContinue expr
    AT.Assignment {} -> EV.generateAssignment expr
    AT.Assembly {} -> EA.generateAssembly expr
