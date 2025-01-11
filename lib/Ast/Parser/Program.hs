module Ast.Parser.Program where

import qualified Ast.Parser.Expr as PE
import qualified Ast.Parser.TypeDefinition as PT
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Text.Megaparsec as M

parseProgram :: String -> PU.Parser AT.Program
parseProgram sourceFile = do
  _ <- PU.sc
  types <- M.many $ M.try $ PU.lexeme PT.parseTypeDefinition
  exprs <- M.many PE.parseExpr
  return $ AT.Program {AT.globals = map globalExpr exprs, AT.types = map globalType types, AT.sourceFile = sourceFile}

globalExpr :: AT.Expr -> (String, AT.Expr)
globalExpr e@(AT.Function {AT.funcName = name}) = (name, e)
globalExpr e@(AT.ForeignFunction {AT.funcName = name}) = (name, e)
globalExpr e@(AT.Declaration {AT.declName = name}) = (name, e)
globalExpr e@(AT.Assignment {AT.assignTarget = (AT.Var _ name _)}) = (name, e)
globalExpr _ = error "invalid global expr"

globalType :: AT.Type -> (String, AT.Type)
globalType t@(AT.TStruct {AT.structName = name}) = (name, t)
globalType t@(AT.TUnion {AT.unionName = name}) = (name, t)
globalType t@(AT.TTypedef name _) = (name, t)
globalType _ = error "invalid global type"
