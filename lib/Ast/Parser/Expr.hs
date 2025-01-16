module Ast.Parser.Expr where

import qualified Ast.Parser.Asm as PA
import qualified Ast.Parser.Literal as PL
import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Type as PT
import qualified Ast.Parser.Utils as AU
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.Combinators.Expr as CE
import qualified Control.Monad.State as S
import qualified Data.Maybe as DM
import qualified Shared.Utils as SU
import qualified Text.Megaparsec as M

parseExpr :: PU.Parser AT.Expr
parseExpr = CE.makeExprParser (PU.lexeme parseTerm) operationTable

operationTable :: [[CE.Operator PU.Parser AT.Expr]]
operationTable =
  [ [ PU.postfix ".*" (`AT.UnaryOp` AT.Deref),
      PU.postfix ".&" (`AT.UnaryOp` AT.AddrOf),
      PU.postfix "++" (`AT.UnaryOp` AT.PostInc),
      PU.postfix "--" (`AT.UnaryOp` AT.PostDec),
      parseCall
    ],
    [ PU.prefix "!" (`AT.UnaryOp` AT.Not),
      PU.prefix "not" (`AT.UnaryOp` AT.Not),
      PU.prefix "~" (`AT.UnaryOp` AT.BitNot),
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
      parseArrayAccess,
      parseStructAccess
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

parseStructAccess :: CE.Operator PU.Parser AT.Expr
parseStructAccess = CE.InfixL $ do
  srcLoc <- PU.parseSrcLoc <* PU.symbol "."
  return $ \value field -> AT.StructAccess srcLoc value field

parseTerm :: PU.Parser AT.Expr
parseTerm =
  M.choice
    [ parseIf,
      parseWhile,
      parseFrom,
      parseReturn,
      parseBreak,
      parseContinue,
      parseBlock id,
      parseCast,
      parseDefer,
      parseAssembly,
      M.try parseFunction,
      M.try parseForeignFunction,
      M.try parseDeclaration,
      M.try parseLit,
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
    _ -> return $ AT.Var srcLoc name AT.TUnknown

parseFunction :: PU.Parser AT.Expr
parseFunction = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  ft <- PU.symbol ":" *> PT.parseType
  case ft of
    (AT.TFunction ret pts _) -> do
      params <- PU.symbol "=" *> M.many (PU.lexeme PU.identifier)
      mapM_ (\(p, t) -> S.modify (PS.insertVar p t)) $ zip params pts
      S.modify (PS.insertVar name ft)
      body <-
        if ret /= AT.TVoid
          then parseBlock implicitReturn
          else parseBlock id
      return $ AT.Function {AT.funcLoc = srcLoc, AT.funcName = name, AT.funcType = ft, AT.funcParams = params, AT.funcBody = body}
    _ -> M.customFailure $ AU.InvalidFunctionType name ft

implicitReturn :: AT.Expr -> AT.Expr
implicitReturn e@(AT.Lit {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Var {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Function {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.ForeignFunction {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Declaration {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Assignment {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Call {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn (AT.If loc cond then' (Just else')) = AT.If loc cond (implicitReturn then') $ Just $ implicitReturn else'
implicitReturn (AT.If loc cond then' Nothing) = AT.If loc cond (implicitReturn then') Nothing
implicitReturn e@(AT.While {}) = e
implicitReturn e@(AT.From {}) = e
implicitReturn (AT.Block []) = AT.Block []
implicitReturn (AT.Block es) = AT.Block $ init es ++ [implicitReturn $ last es]
implicitReturn e@(AT.Return _ _) = e
implicitReturn e@(AT.Break {}) = e
implicitReturn e@(AT.Continue {}) = e
implicitReturn e@(AT.Op {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.UnaryOp {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.StructAccess {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.ArrayAccess {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Cast {}) = AT.Return (SU.getLoc e) $ Just e
implicitReturn e@(AT.Assembly {}) = AT.Return (SU.getLoc e) $ Just e

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
  then' <- parseBlock id
  else' <- M.optional $ PU.symbol "else" *> parseBlock id
  return $ AT.If {AT.ifLoc = srcLoc, AT.ifCond = cond, AT.ifThen = then', AT.ifElse = else'}

parseWhile :: PU.Parser AT.Expr
parseWhile = do
  srcLoc <- PU.parseSrcLoc
  cond <- PU.symbol "loop" *> parseExpr
  body <- parseBlock id
  return $ AT.While {AT.whileLoc = srcLoc, AT.whileCond = cond, AT.whileBody = body}

parseFrom :: PU.Parser AT.Expr
parseFrom = do
  srcLoc <- PU.parseSrcLoc
  start <- PU.symbol "from" *> PU.lexeme parseExpr
  end <- PU.symbol "to" *> PU.lexeme parseExpr
  step <- M.optional $ M.try $ PU.symbol "by" *> PU.lexeme parseExpr
  (name, type') <-
    M.between (PU.symbol "[") (PU.symbol "]") $ do
      name <- PU.identifier
      type' <- PU.symbol ":" *> PT.parseType
      return (name, type')
  let decl = AT.Declaration srcLoc name type' $ Just start
  let var = AT.Var srcLoc name type'
  let one = AT.Lit srcLoc (AT.LInt 1)
  let stepExpr = AT.Assignment srcLoc var $ AT.Op srcLoc AT.Add var $ DM.fromMaybe one step
  S.modify (PS.insertVar name type')
  body <- parseBlock id
  return $ AT.From srcLoc start end stepExpr decl body

parseBlock :: (AT.Expr -> AT.Expr) -> PU.Parser AT.Expr
parseBlock f = do
  _ <- PU.symbol "{"
  outerState <- S.get
  S.modify PS.pushScope
  es <- M.many $ PU.lexeme parseExpr
  _ <- PU.symbol "}"
  blockState <- S.get
  let (deferred, ds) = PS.popScope blockState
  S.put outerState {PS.deferState = PS.deferState ds}
  return $ deferedExpr deferred . f $ AT.Block es

deferedExpr :: [AT.Expr] -> AT.Expr -> AT.Expr
deferedExpr ds (AT.Block []) = AT.Block ds
deferedExpr ds (AT.Block es) = case last es of
  e@(AT.Return _ _) -> AT.Block $ init es ++ ds ++ [e]
  _ -> AT.Block $ es ++ ds
deferedExpr _ e = e

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

parseCast :: PU.Parser AT.Expr
parseCast = do
  srcLoc <- PU.parseSrcLoc
  type' <- PU.symbol "@" *> PT.parseType
  expr <- M.between (PU.symbol "(") (PU.symbol ")") parseExpr
  return $ AT.Cast srcLoc type' expr

parseDefer :: PU.Parser AT.Expr
parseDefer = do
  defered <- PU.symbol "defer" *> parseExpr
  S.modify $ PS.pushDefered defered
  next <- M.optional parseExpr
  case next of
    Nothing -> M.customFailure $ PU.InvalidDefer defered
    (Just e) -> return e

parseParenExpr :: PU.Parser AT.Expr
parseParenExpr = M.between (PU.symbol "(") (PU.symbol ")") parseExpr

parseAssembly :: PU.Parser AT.Expr
parseAssembly = do
  srcLoc <- PU.parseSrcLoc
  AT.Assembly srcLoc <$> (PU.symbol "__asm__" *> PA.parseAsm parseExpr PT.parseType)
