module Ast.Parser.Expr where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.Operation as PO
import qualified Ast.Parser.Type as PT
import qualified Ast.Parser.UnaryOperation as PUO
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Pos as MP

-- TODO: rethink order
parseExpr :: PU.Parser AT.Expr
parseExpr = PU.triedChoice [parseLit, parseVar, parseFunction, parseDeclaration, parseAssignment, parseCall, parseIf, parseBlock, parseOp]

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

parseFunction :: PU.Parser AT.Expr
parseFunction = do
  name <- PU.identifier
  t <- PU.symbol ":" *> PT.parseType
  params <- PU.symbol "=" *> M.many PU.identifier
  body <- parseBlock
  srcLoc <- parseSrcLoc
  S.modify (E.insertVar name t)
  return $ AT.Function {AT.funcLoc = srcLoc, AT.funcName = name, AT.funcType = t, AT.funcParams = params, AT.funcBody = body}

parseDeclaration :: PU.Parser AT.Expr
parseDeclaration = do
  name <- PU.lexeme PU.identifier
  t <- PU.symbol ":" *> PT.parseType
  value <- M.optional $ PU.symbol "=" *> parseExpr
  srcLoc <- parseSrcLoc
  S.modify (E.insertVar name t)
  return $ AT.Declaration {AT.declLoc = srcLoc, AT.declName = name, AT.declType = t, AT.declInit = value}

parseAssignment :: PU.Parser AT.Expr
parseAssignment = do
  target <- parseExpr <* PU.symbol "="
  value <- parseExpr
  srcLoc <- parseSrcLoc
  return $ AT.Assignment {AT.assignLoc = srcLoc, AT.assignTarget = target, AT.assignValue = value}

parseCall :: PU.Parser AT.Expr
parseCall = do
  name <- PU.identifier
  args <- M.between (PU.symbol "(") (PU.symbol ")") $ M.many parseExpr
  srcLoc <- parseSrcLoc
  env <- S.get
  case E.lookupVar name env of
    (Just t@(AT.TFunction {})) -> return $ AT.Call {AT.callLoc = srcLoc, AT.callFunc = AT.Var srcLoc name t, AT.callArgs = args}
    _ -> M.customFailure $ PU.UndefinedFunction name

parseIf :: PU.Parser AT.Expr
parseIf = do
  cond <- PU.symbol "if" *> parseExpr
  then' <- parseBlock
  else' <- M.optional $ PU.symbol "else" *> parseBlock
  srcLoc <- parseSrcLoc
  return $ AT.If {AT.ifLoc = srcLoc, AT.ifCond = cond, AT.ifThen = then', AT.ifElse = else'}

parseBlock :: PU.Parser AT.Expr
parseBlock = do
  es <- M.between (PU.symbol "{") (PU.symbol "}") $ M.many parseExpr
  return $ AT.Block es

parseOp :: PU.Parser AT.Expr
parseOp = do
  e1 <- parseExpr
  op <- PO.parseOperation
  e2 <- parseExpr
  srcLoc <- parseSrcLoc
  return $ AT.Op srcLoc op e1 e2

parseUnaryOp :: PU.Parser AT.Expr
parseUnaryOp = do
  uoType <- PUO.unaryOperationType parseExpr
  case uoType of
    PUO.Pre -> do
      uo <- PUO.parseUnaryOperation uoType
      e <- parseExpr
      srcLoc <- parseSrcLoc
      return $ AT.UnaryOp srcLoc uo e
    PUO.Post -> do
      e <- parseExpr
      uo <- PUO.parseUnaryOperation uoType
      srcLoc <- parseSrcLoc
      return $ AT.UnaryOp srcLoc uo e

parseSrcLoc :: PU.Parser AT.SrcLoc
parseSrcLoc = do
  (MP.SourcePos {MP.sourceName = _sourceName, MP.sourceLine = _sourceLine, MP.sourceColumn = _sourceColumn}) <- M.getSourcePos
  return $ AT.SrcLoc {AT.srcFile = _sourceName, AT.srcLine = MP.unPos _sourceLine, AT.srcCol = MP.unPos _sourceColumn}
