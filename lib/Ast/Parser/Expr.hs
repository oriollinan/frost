module Ast.Parser.Expr where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.Operation as PO
import qualified Ast.Parser.Type as PT
import qualified Ast.Parser.UnaryOperation as PUO
import qualified Ast.Parser.Utils as AU
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Pos as MP

-- TODO: rethink order
parseExpr :: PU.Parser AT.Expr
parseExpr =
  M.choice
    [ parseIf,
      parseWhile,
      parseFor,
      parseReturn,
      parseBreak,
      parseContinue,
      parseBlock,
      parseCast,
      parseLit,
      M.try parseFunction,
      M.try parseDeclaration,
      M.try parseAssignment,
      M.try parseCall,
      M.try parseStructAccess,
      M.try parseArrayAccess,
      parseVar,
      parseOp
      -- parseUnaryOp,
    ]

parseLit :: PU.Parser AT.Expr
parseLit = do
  srcLoc <- parseSrcLoc
  AT.Lit srcLoc <$> PL.parseLiteral

parseVar :: PU.Parser AT.Expr
parseVar = do
  srcLoc <- parseSrcLoc
  name <- PU.identifier
  env <- S.get
  case E.lookupVar name env of
    (Just t) -> return $ AT.Var srcLoc name t
    _ -> M.customFailure $ PU.UndefinedVar name

-- TODO: improve implicit return to handle ifs
parseFunction :: PU.Parser AT.Expr
parseFunction = do
  srcLoc <- parseSrcLoc
  name <- PU.identifier
  ft <- PU.symbol ":" *> PT.parseType
  case ft of
    (AT.TFunction {AT.paramTypes = pts}) -> do
      params <- PU.symbol "=" *> M.many (PU.lexeme PU.identifier)
      mapM_ (\(p, t) -> S.modify (E.insertVar p t)) $ zip params pts
      S.modify (E.insertVar name ft)
      (AT.Block exprs) <- parseBlock
      body <- case last exprs of
        (AT.Return _ _) -> return $ AT.Block exprs
        e -> return $ AT.Block $ init exprs ++ [AT.Return srcLoc $ Just e]
      return $ AT.Function {AT.funcLoc = srcLoc, AT.funcName = name, AT.funcType = ft, AT.funcParams = params, AT.funcBody = body}
    _ -> M.customFailure $ AU.InvalidFunctionType name ft

parseDeclaration :: PU.Parser AT.Expr
parseDeclaration = do
  srcLoc <- parseSrcLoc
  name <- PU.identifier
  t <- PU.symbol ":" *> PT.parseType
  value <- M.optional $ PU.symbol "=" *> parseExpr
  S.modify (E.insertVar name t)
  return $ AT.Declaration {AT.declLoc = srcLoc, AT.declName = name, AT.declType = t, AT.declInit = value}

parseAssignment :: PU.Parser AT.Expr
parseAssignment = do
  srcLoc <- parseSrcLoc
  target <- parseVar <* PU.symbol "="
  value <- parseExpr
  return $ AT.Assignment {AT.assignLoc = srcLoc, AT.assignTarget = target, AT.assignValue = value}

parseCall :: PU.Parser AT.Expr
parseCall = do
  srcLoc <- parseSrcLoc
  name <- PU.identifier
  args <- M.between (PU.symbol "(") (PU.symbol ")") $ M.many parseExpr
  env <- S.get
  case E.lookupVar name env of
    (Just t@(AT.TFunction {})) -> return $ AT.Call {AT.callLoc = srcLoc, AT.callFunc = AT.Var srcLoc name t, AT.callArgs = args}
    _ -> M.customFailure $ PU.UndefinedFunction name

parseIf :: PU.Parser AT.Expr
parseIf = do
  srcLoc <- parseSrcLoc
  cond <- PU.symbol "if" *> parseExpr
  then' <- parseBlock
  else' <- M.optional $ PU.symbol "else" *> parseBlock
  return $ AT.If {AT.ifLoc = srcLoc, AT.ifCond = cond, AT.ifThen = then', AT.ifElse = else'}

parseWhile :: PU.Parser AT.Expr
parseWhile = do
  srcLoc <- parseSrcLoc
  cond <- PU.symbol "loop" *> parseExpr
  body <- parseBlock
  return $ AT.While {AT.whileLoc = srcLoc, AT.whileCond = cond, AT.whileBody = body}

-- TODO: handle dynamic ranges
parseFor :: PU.Parser AT.Expr
parseFor = do
  srcLoc <- parseSrcLoc
  from <- PU.symbol "from" *> PU.lexeme parseExpr
  to <- PU.symbol "to" *> PU.lexeme parseExpr
  by <- M.optional $ PU.symbol "by" *> PU.lexeme ML.decimal
  (name, type') <- M.between (PU.symbol "|") (PU.symbol "|") $ do
    name <- PU.identifier
    type' <- PU.symbol ":" *> PT.parseType
    return (name, type')
  let init' = AT.Declaration srcLoc name type' (Just from)
  let var = AT.Var srcLoc name type'
  S.modify (E.insertVar name type')
  body <- parseBlock
  let step = case by of
        Just n -> AT.Assignment srcLoc var (AT.Op srcLoc AT.Add var (AT.Lit srcLoc (AT.LInt n)))
        _ -> AT.UnaryOp srcLoc AT.PostInc var
  let cond = (if maybe True (>= 0) by then AT.Op srcLoc AT.Lt var to else AT.Op srcLoc AT.Gt var to)
  return $ AT.For {AT.forLoc = srcLoc, AT.forInit = init', AT.forCond = cond, AT.forStep = step, AT.forBody = body}

parseBlock :: PU.Parser AT.Expr
parseBlock = do
  env <- S.get
  es <- M.between (PU.symbol "{") (PU.symbol "}") $ M.many $ PU.lexeme parseExpr
  S.modify $ const env
  return $ AT.Block es

parseReturn :: PU.Parser AT.Expr
parseReturn = do
  srcLoc <- parseSrcLoc
  _ <- PU.symbol "return"
  AT.Return srcLoc <$> M.optional parseExpr

parseBreak :: PU.Parser AT.Expr
parseBreak = do
  srcLoc <- parseSrcLoc
  _ <- PU.symbol "stop"
  return $ AT.Break srcLoc

parseContinue :: PU.Parser AT.Expr
parseContinue = do
  srcLoc <- parseSrcLoc
  _ <- PU.symbol "next"
  return $ AT.Continue srcLoc

parseOp :: PU.Parser AT.Expr
parseOp = do
  srcLoc <- parseSrcLoc
  op <- PO.parseOperation
  e1 <- parseExpr
  AT.Op srcLoc op e1 <$> parseExpr

parseUnaryOp :: PU.Parser AT.Expr
parseUnaryOp = do
  srcLoc <- parseSrcLoc
  (uo, e) <- PUO.parseUnaryOperation parseExpr
  return $ AT.UnaryOp srcLoc uo e

-- TODO: parse nested structs
parseStructAccess :: PU.Parser AT.Expr
parseStructAccess = do
  srcLoc <- parseSrcLoc
  value <- parseVar
  field <- PU.symbol "." *> PU.identifier
  return $ AT.StructAccess srcLoc value field

-- TODO: parse nested arrays
parseArrayAccess :: PU.Parser AT.Expr
parseArrayAccess = do
  srcLoc <- parseSrcLoc
  value <- parseVar
  pos <- PU.symbol "." *> parseExpr
  return $ AT.ArrayAccess srcLoc value pos

parseCast :: PU.Parser AT.Expr
parseCast = do
  srcLoc <- parseSrcLoc
  type' <- PU.symbol "@" *> PT.parseType
  expr <- M.between (PU.symbol "(") (PU.symbol ")") parseExpr
  return $ AT.Cast srcLoc type' expr

parseSrcLoc :: PU.Parser AT.SrcLoc
parseSrcLoc = do
  (MP.SourcePos {MP.sourceName = _sourceName, MP.sourceLine = _sourceLine, MP.sourceColumn = _sourceColumn}) <- M.getSourcePos
  return $ AT.SrcLoc {AT.srcFile = _sourceName, AT.srcLine = MP.unPos _sourceLine, AT.srcCol = MP.unPos _sourceColumn}
