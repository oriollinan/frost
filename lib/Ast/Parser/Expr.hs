module Ast.Parser.Expr where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.Type as PT
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Pos as MP

parseExpr :: PU.Parser AT.Expr
parseExpr = PU.triedChoice [parseLit, parseVar, parseDeclaration]

parseLit :: PU.Parser AT.Expr
parseLit = do
  lit <- PL.parseLiteral
  srcLoc <- parseSrcLoc
  return $ AT.Lit srcLoc lit

parseVar :: PU.Parser AT.Expr
parseVar = do
  name <- PU.identifier
  srcLoc <- parseSrcLoc
  env <- S.get
  case E.lookupVar name env of
    (Just t) -> return $ AT.Var srcLoc name t
    _ -> M.customFailure $ PU.UndefinedVar name

parseDeclaration :: PU.Parser AT.Expr
parseDeclaration = do
  name <- PU.lexeme PU.identifier
  t <- PU.symbol ":" *> PT.parseType
  value <- M.optional $ PU.symbol "=" *> parseExpr
  srcLoc <- parseSrcLoc
  S.modify (E.insertVar name t)
  return $ AT.Declaration {AT.declLoc = srcLoc, AT.declName = name, AT.declType = t, AT.declInit = value}

parseSrcLoc :: PU.Parser AT.SrcLoc
parseSrcLoc = do
  (MP.SourcePos {MP.sourceName = _sourceName, MP.sourceLine = _sourceLine, MP.sourceColumn = _sourceColumn}) <- M.getSourcePos
  return $ AT.SrcLoc {AT.srcFile = _sourceName, AT.srcLine = MP.unPos _sourceLine, AT.srcCol = MP.unPos _sourceColumn}
