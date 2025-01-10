module Ast.Parser.Expr where

import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Type as PT
import qualified Ast.Parser.Utils as AU
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.Combinators.Expr as CE
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char.Lexer as ML

parseExpr :: PU.Parser AT.Expr
parseExpr = CE.makeExprParser (PU.lexeme parseTerm) operationTable

operationTable :: [[CE.Operator PU.Parser AT.Expr]]
operationTable =
  [ [ PU.postfix ".*" (`AT.UnaryOp` AT.Deref),
      PU.postfix "++" (`AT.UnaryOp` AT.PostInc),
      PU.postfix "--" (`AT.UnaryOp` AT.PostDec),
      parseCall
    ],
    [ PU.prefix "!" (`AT.UnaryOp` AT.Not),
      PU.prefix "not" (`AT.UnaryOp` AT.Not),
      PU.prefix "~" (`AT.UnaryOp` AT.BitNot),
      PU.prefix "&" (`AT.UnaryOp` AT.AddrOf),
      PU.prefix "++" (`AT.UnaryOp` AT.PreInc),
      PU.prefix "--" (`AT.UnaryOp` AT.PreDec)
    ],
    [ PU.binary "*" (`AT.Op` AT.Mul),
      PU.binary "/" (`AT.Op` AT.Div),
      PU.binary "mod" (`AT.Op` AT.Mod)
    ],
    [ PU.binary "+" (`AT.Op` AT.Add),
      PU.binary "-" (`AT.Op` AT.Sub)
    ],
    [ PU.binary "&" (`AT.Op` AT.BitAnd),
      PU.binary "|" (`AT.Op` AT.BitOr),
      PU.binary "^" (`AT.Op` AT.BitXor),
      PU.binary "<<" (`AT.Op` AT.BitShl),
      PU.binary ">>" (`AT.Op` AT.BitShr)
    ],
    [ PU.binary "==" (`AT.Op` AT.Eq),
      PU.binary "is" (`AT.Op` AT.Eq),
      PU.binary "!=" (`AT.Op` AT.Ne),
      PU.binary "<=" (`AT.Op` AT.Lte),
      PU.binary ">=" (`AT.Op` AT.Gte),
      PU.binary "<" (`AT.Op` AT.Lt),
      PU.binary ">" (`AT.Op` AT.Gt),
      parseAssignment,
      parseArrayAccess
    ],
    [ PU.binary "&&" (`AT.Op` AT.And),
      PU.binary "and" (`AT.Op` AT.And),
      PU.binary "||" (`AT.Op` AT.Or),
      PU.binary "or" (`AT.Op` AT.Or)
    ]
  ]

parseCall :: CE.Operator PU.Parser AT.Expr
parseCall = CE.Postfix $ do
  srcLoc <- PU.parseSrcLoc
  args <- M.between (PU.symbol "(") (PU.symbol ")") $ M.many parseExpr
  return (\func -> AT.Call srcLoc func args)

parseAssignment :: CE.Operator PU.Parser AT.Expr
parseAssignment = CE.InfixL $ do
  srcLoc <- PU.parseSrcLoc <* PU.symbol "="
  return $ \target value -> AT.Assignment srcLoc target value

parseArrayAccess :: CE.Operator PU.Parser AT.Expr
parseArrayAccess = CE.InfixL $ do
  srcLoc <- PU.parseSrcLoc <* PU.symbol ".#"
  return $ \value pos -> AT.ArrayAccess srcLoc value pos

parseTerm :: PU.Parser AT.Expr
parseTerm =
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
      M.try parseForeignFunction,
      M.try parseDeclaration,
      M.try parseStructAccess,
      parseVar,
      parseParenExpr
    ]

parseLit :: PU.Parser AT.Expr
parseLit = do
  srcLoc <- PU.parseSrcLoc
  AT.Lit srcLoc <$> PL.parseLiteral

parseVar :: PU.Parser AT.Expr
parseVar = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  env <- S.get
  case PS.lookupVar name env of
    (Just t) -> return $ AT.Var srcLoc name t
    _ -> M.customFailure $ PU.UndefinedVar name

parseFunction :: PU.Parser AT.Expr
parseFunction = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  ft <- PU.symbol ":" *> PT.parseType
  case ft of
    (AT.TFunction {AT.paramTypes = pts}) -> do
      params <- PU.symbol "=" *> M.many (PU.lexeme PU.identifier)
      mapM_ (\(p, t) -> S.modify (PS.insertVar p t)) $ zip params pts
      S.modify (PS.insertVar name ft)
      block <- parseBlock
      let body = implicitReturn block
      return $ AT.Function {AT.funcLoc = srcLoc, AT.funcName = name, AT.funcType = ft, AT.funcParams = params, AT.funcBody = body}
    _ -> M.customFailure $ AU.InvalidFunctionType name ft

parseForeignFunction :: PU.Parser AT.Expr
parseForeignFunction = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  ft <- PU.symbol ":" *> PU.symbol "foreign" *> PU.lexeme PT.parseType
  case ft of
    t@(AT.TFunction {}) -> do
      S.modify (PS.insertVar name t)
      return $
        AT.ForeignFunction
          { AT.funcLoc = srcLoc,
            AT.funcName = name,
            AT.funcType = t
          }
    _ -> M.customFailure $ AU.InvalidFunctionType name ft

implicitReturn :: AT.Expr -> AT.Expr
implicitReturn e@(AT.Lit loc _) = AT.Return loc $ Just e
implicitReturn e@(AT.Var loc _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.Function loc _ _ _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.ForeignFunction loc _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.Declaration loc _ _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.Assignment loc _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.Call loc _ _) = AT.Return loc $ Just e
implicitReturn (AT.If loc cond then' (Just else')) = AT.If loc cond (implicitReturn then') $ Just $ implicitReturn else'
implicitReturn (AT.If loc cond then' Nothing) = AT.If loc cond (implicitReturn then') Nothing
implicitReturn e@(AT.While {}) = e
implicitReturn e@(AT.For {}) = e
implicitReturn (AT.Block es) = AT.Block $ init es ++ [implicitReturn $ last es]
implicitReturn e@(AT.Return _ _) = e
implicitReturn e@(AT.Break {}) = e
implicitReturn e@(AT.Continue {}) = e
implicitReturn e@(AT.Op loc _ _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.UnaryOp loc _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.StructAccess loc _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.ArrayAccess loc _ _) = AT.Return loc $ Just e
implicitReturn e@(AT.Cast loc _ _) = AT.Return loc $ Just e

parseDeclaration :: PU.Parser AT.Expr
parseDeclaration = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  t <- PU.symbol ":" *> PT.parseType
  value <- M.optional $ PU.symbol "=" *> parseExpr
  S.modify (PS.insertVar name t)
  return $ AT.Declaration {AT.declLoc = srcLoc, AT.declName = name, AT.declType = t, AT.declInit = value}

parseIf :: PU.Parser AT.Expr
parseIf = do
  srcLoc <- PU.parseSrcLoc
  cond <- PU.symbol "if" *> PU.lexeme parseExpr
  then' <- parseBlock
  else' <- M.optional $ PU.symbol "else" *> parseBlock
  return $ AT.If {AT.ifLoc = srcLoc, AT.ifCond = cond, AT.ifThen = then', AT.ifElse = else'}

parseWhile :: PU.Parser AT.Expr
parseWhile = do
  srcLoc <- PU.parseSrcLoc
  cond <- PU.symbol "loop" *> parseExpr
  body <- parseBlock
  return $ AT.While {AT.whileLoc = srcLoc, AT.whileCond = cond, AT.whileBody = body}

-- TODO: handle dynamic ranges
parseFor :: PU.Parser AT.Expr
parseFor = do
  srcLoc <- PU.parseSrcLoc
  from <- PU.symbol "from" *> PU.lexeme parseExpr
  to <- PU.symbol "to" *> PU.lexeme parseExpr
  by <- M.optional $ PU.symbol "by" *> PU.lexeme ML.decimal
  (name, type') <- M.between (PU.symbol "|") (PU.symbol "|") $ do
    name <- PU.identifier
    type' <- PU.symbol ":" *> PT.parseType
    return (name, type')
  let init' = AT.Declaration srcLoc name type' (Just from)
  let var = AT.Var srcLoc name type'
  S.modify (PS.insertVar name type')
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
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "ret"
  AT.Return srcLoc <$> M.optional parseExpr

parseBreak :: PU.Parser AT.Expr
parseBreak = do
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "stop"
  return $ AT.Break srcLoc

parseContinue :: PU.Parser AT.Expr
parseContinue = do
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "next"
  return $ AT.Continue srcLoc

-- TODO: parse nested structs
parseStructAccess :: PU.Parser AT.Expr
parseStructAccess = do
  srcLoc <- PU.parseSrcLoc
  value <- parseVar
  field <- PU.symbol "." *> PU.identifier
  return $ AT.StructAccess srcLoc value field

parseCast :: PU.Parser AT.Expr
parseCast = do
  srcLoc <- PU.parseSrcLoc
  type' <- PU.symbol "@" *> PT.parseType
  expr <- M.between (PU.symbol "(") (PU.symbol ")") parseExpr
  return $ AT.Cast srcLoc type' expr

parseParenExpr :: PU.Parser AT.Expr
parseParenExpr = M.between (PU.symbol "(") (PU.symbol ")") parseExpr
