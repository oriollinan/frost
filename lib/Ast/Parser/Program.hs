module Ast.Parser.Program where

import qualified Ast.Parser.Expr as PE
import qualified Ast.Parser.PreProcessor as PP
import qualified Ast.Parser.TypeDefinition as PT
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Text.Megaparsec as M

-- | Parses a program by first preprocessing the source file and then parsing its components.
-- Components can be type definitions or expressions.
-- Returns an `AT.Program` containing the parsed global expressions, types, and the source file name.
parseProgram :: String -> PU.Parser AT.Program
parseProgram sourceFile = do
  source <- PP.preprocess sourceFile
  M.setInput source
  _ <- PU.sc
  components <- M.many $ M.choice [M.try parseTypeDefinition, parseExpr]
  return $ AT.Program (concatMap AT.globals components) (concatMap AT.types components) sourceFile

-- | Parses a type definition and wraps it in an `AT.Program` structure.
-- Returns a program with the parsed type definition as a global type.
parseTypeDefinition :: PU.Parser AT.Program
parseTypeDefinition = do
  type' <- PU.lexeme PT.parseTypeDefinition
  return $ AT.Program [] [globalType type'] ""

-- | Parses an expression and wraps it in an `AT.Program` structure.
-- Returns a program with the parsed expression as a global expression.
parseExpr :: PU.Parser AT.Program
parseExpr = do
  expr <- PE.parseExpr
  return $ AT.Program [globalExpr expr] [] ""

-- | Converts a parsed expression into a global expression.
-- Returns a tuple containing the name of the global expression and the expression itself.
globalExpr :: AT.Expr -> (String, AT.Expr)
globalExpr e@(AT.Function {AT.funcName = name}) = (name, e)
globalExpr e@(AT.ForeignFunction {AT.funcName = name}) = (name, e)
globalExpr e@(AT.Declaration {AT.declName = name}) = (name, e)
globalExpr e@(AT.Assignment {AT.assignTarget = (AT.Var _ name _)}) = (name, e)
globalExpr e = error $ "invalid global expr: " ++ show e

-- | Converts a parsed type into a global type.
-- Returns a tuple containing the name of the global type and the type itself.
globalType :: AT.Type -> (String, AT.Type)
globalType t@(AT.TStruct {AT.structName = name}) = (name, t)
globalType t@(AT.TUnion {AT.unionName = name}) = (name, t)
globalType t@(AT.TTypedef name _) = (name, t)
globalType t = error $ "invalid global type: " ++ show t
