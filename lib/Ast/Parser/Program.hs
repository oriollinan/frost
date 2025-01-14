module Ast.Parser.Program where

import qualified Ast.Parser.Expr as PE
import qualified Ast.Parser.Import as PI
import qualified Ast.Parser.State as PS
import qualified Ast.Parser.TypeDefinition as PT
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

parseProgram :: String -> PU.Parser AT.Program
parseProgram sourceFile = do
  _ <- PU.sc
  S.modify $ PS.insertImport sourceFile
  source <- preprocess sourceFile
  M.setInput source
  components <- M.many $ M.choice [M.try parseTypeDefinition, parseExpr]
  return $ AT.Program (concatMap AT.globals components) (concatMap AT.types components) sourceFile

preprocess :: String -> PU.Parser String
preprocess sourceFile = do
  sources <- M.many $ M.choice [PI.parseImport sourceFile (preprocess sourceFile), (: []) <$> M.anySingle]
  return $ concat sources

parseTypeDefinition :: PU.Parser AT.Program
parseTypeDefinition = do
  type' <- PU.lexeme PT.parseTypeDefinition
  return $ AT.Program [] [globalType type'] ""

parseExpr :: PU.Parser AT.Program
parseExpr = do
  expr <- PE.parseExpr
  return $ AT.Program [globalExpr expr] [] ""

globalExpr :: AT.Expr -> (String, AT.Expr)
globalExpr e@(AT.Function {AT.funcName = name}) = (name, e)
globalExpr e@(AT.ForeignFunction {AT.funcName = name}) = (name, e)
globalExpr e@(AT.Declaration {AT.declName = name}) = (name, e)
globalExpr e@(AT.Assignment {AT.assignTarget = (AT.Var _ name _)}) = (name, e)
globalExpr e = error $ "invalid global expr" ++ show e

globalType :: AT.Type -> (String, AT.Type)
globalType t@(AT.TStruct {AT.structName = name}) = (name, t)
globalType t@(AT.TUnion {AT.unionName = name}) = (name, t)
globalType t@(AT.TTypedef name _) = (name, t)
globalType _ = error "invalid global type"
