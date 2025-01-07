module Ast.Parser.Expr where

import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Pos as MP

parseExpr :: PU.Parser AT.Expr
parseExpr = PU.triedChoice [parseLit]

parseLit :: PU.Parser AT.Expr
parseLit = do
  lit <- PL.parseLiteral
  srcLoc <- parseSrcLoc
  return $ AT.Lit srcLoc lit

parseSrcLoc :: PU.Parser AT.SrcLoc
parseSrcLoc = do
  (MP.SourcePos {MP.sourceName = _sourceName, MP.sourceLine = _sourceLine, MP.sourceColumn = _sourceColumn}) <- M.getSourcePos
  return $ AT.SrcLoc {AT.srcLine = MP.unPos _sourceLine, AT.srcFile = _sourceName, AT.srcCol = MP.unPos _sourceColumn}
