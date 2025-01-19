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

-- | Parses an expression using a precedence-aware parser.
-- Returns an `AT.Expr` representing the parsed expression.
parseExpr :: PU.Parser AT.Expr
parseExpr = CE.makeExprParser (PU.lexeme parseTerm) operationTable

-- | Defines the operation table for parsing expressions.
-- Includes prefix, postfix, and infix operators with their precedence levels.
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
    [ PU.binary "and" (`AT.Op` AT.And),
      PU.binary "or" (`AT.Op` AT.Or)
    ]
  ]

-- | Parses a function or method call expression.
-- Returns a `Postfix` operator that adds the call to an existing expression.
parseCall :: CE.Operator PU.Parser AT.Expr
parseCall = CE.Postfix $ do
  srcLoc <- PU.parseSrcLoc
  args <- M.between (PU.symbol "(") (PU.symbol ")") $ M.many parseExpr
  return (\func -> AT.Call srcLoc func args)

-- | Parses an assignment expression.
-- Returns an `InfixL` operator that creates an `AT.Assignment`.
parseAssignment :: CE.Operator PU.Parser AT.Expr
parseAssignment = CE.InfixL $ do
  srcLoc <- PU.parseSrcLoc <* PU.symbol "="
  return $ \target value -> AT.Assignment srcLoc target value

-- | Parses an array access expression.
-- Returns an `InfixL` operator that creates an `AT.ArrayAccess`.
parseArrayAccess :: CE.Operator PU.Parser AT.Expr
parseArrayAccess = CE.InfixL $ do
  srcLoc <- PU.parseSrcLoc <* PU.symbol ".#"
  return $ \value pos -> AT.ArrayAccess srcLoc value pos

-- | Parses a structure field access expression.
-- Returns an `InfixL` operator that creates an `AT.StructAccess`.
parseStructAccess :: CE.Operator PU.Parser AT.Expr
parseStructAccess = CE.InfixL $ do
  srcLoc <- PU.parseSrcLoc <* PU.symbol "."
  return $ \value field -> AT.StructAccess srcLoc value field

-- | Parses a term, which can be a variety of expressions, including:
-- literals, variables, blocks, loops, if statements, function declarations, and more.
-- Returns the parsed `AT.Expr`.
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

-- | Parses a literal (e.g., integers, strings, booleans).
-- Returns an `AT.Lit` containing the parsed literal.
parseLit :: PU.Parser AT.Expr
parseLit = do
  srcLoc <- PU.parseSrcLoc
  AT.Lit srcLoc <$> PL.parseLiteral

-- | Parses a variable reference.
-- Returns an `AT.Var` containing the variable name and type.
parseVar :: PU.Parser AT.Expr
parseVar = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  env <- S.get
  case PS.lookupVar name env of
    (Just t) -> return $ AT.Var srcLoc name t
    _ -> return $ AT.Var srcLoc name AT.TUnknown

-- | Parses a function declaration, including its name, type, parameters, and body.
-- Returns an `AT.Function`.
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

-- | Adds an implicit return statement to the last expression in a block if needed.
-- Returns the modified `AT.Expr`.
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

-- | Parses a foreign function declaration, which includes the `foreign` keyword.
-- Returns an `AT.ForeignFunction`.
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

-- | Parses a variable declaration, including its name, type, and optional initializer.
-- Returns an `AT.Declaration`.
parseDeclaration :: PU.Parser AT.Expr
parseDeclaration = do
  srcLoc <- PU.parseSrcLoc
  name <- PU.identifier
  t <- PU.symbol ":" *> PT.parseType
  value <- M.optional $ PU.symbol "=" *> parseExpr
  S.modify (PS.insertVar name t)
  return $ AT.Declaration {AT.declLoc = srcLoc, AT.declName = name, AT.declType = t, AT.declInit = value}

-- | Parses an `if` expression, including its condition, `then` block, and optional `else` block.
-- Returns an `AT.If`.
parseIf :: PU.Parser AT.Expr
parseIf = do
  srcLoc <- PU.parseSrcLoc
  cond <- PU.symbol "if" *> PU.lexeme parseExpr
  then' <- parseBlock id
  else' <- M.optional $ PU.symbol "else" *> parseBlock id
  return $ AT.If {AT.ifLoc = srcLoc, AT.ifCond = cond, AT.ifThen = then', AT.ifElse = else'}

-- | Parses a `while` loop expression, including its condition and body.
-- Returns an `AT.While`.
parseWhile :: PU.Parser AT.Expr
parseWhile = do
  srcLoc <- PU.parseSrcLoc
  cond <- PU.symbol "loop" *> parseExpr
  body <- parseBlock id
  return $ AT.While {AT.whileLoc = srcLoc, AT.whileCond = cond, AT.whileBody = body}

-- | Parses a `from` loop expression, including the range and step configuration.
-- Returns an `AT.From`.
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

-- | Parses a block of expressions enclosed by `{}`.
-- Accepts a transformation function to apply to the block's result.
-- Returns an `AT.Block` or the transformed expression.
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

-- | Combines deferred expressions with the main block.
-- Ensures that deferred expressions are executed in the correct order.
-- Returns the modified `AT.Expr`.
deferedExpr :: [AT.Expr] -> AT.Expr -> AT.Expr
deferedExpr ds (AT.Block []) = AT.Block ds
deferedExpr ds (AT.Block es) = case last es of
  e@(AT.Return _ _) -> AT.Block $ init es ++ ds ++ [e]
  _ -> AT.Block $ es ++ ds
deferedExpr _ e = e

-- | Parses a `return` statement.
-- Returns an `AT.Return` with an optional expression.
parseReturn :: PU.Parser AT.Expr
parseReturn = do
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "ret"
  AT.Return srcLoc <$> M.optional parseExpr

-- | Parses a `break` statement.
-- Returns an `AT.Break`.
parseBreak :: PU.Parser AT.Expr
parseBreak = do
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "stop"
  return $ AT.Break srcLoc

-- | Parses a `continue` statement.
-- Returns an `AT.Continue`.
parseContinue :: PU.Parser AT.Expr
parseContinue = do
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "next"
  return $ AT.Continue srcLoc

-- | Parses a type cast expression.
-- Returns an `AT.Cast`.
parseCast :: PU.Parser AT.Expr
parseCast = do
  srcLoc <- PU.parseSrcLoc
  type' <- PU.symbol "@" *> PT.parseType
  expr <- M.between (PU.symbol "(") (PU.symbol ")") parseExpr
  return $ AT.Cast srcLoc type' expr

-- | Parses a `defer` statement, which registers an expression to be executed later.
-- Returns the next expression to be parsed.
parseDefer :: PU.Parser AT.Expr
parseDefer = do
  defered <- PU.symbol "defer" *> parseExpr
  S.modify $ PS.pushDefered defered
  next <- M.optional parseExpr
  case next of
    Nothing -> M.customFailure $ PU.InvalidDefer defered
    (Just e) -> return e

-- | Parses an expression enclosed in parentheses.
-- Returns the parsed `AT.Expr`.
parseParenExpr :: PU.Parser AT.Expr
parseParenExpr = M.between (PU.symbol "(") (PU.symbol ")") parseExpr

-- | Parses an inline assembly block using the `__asm__` keyword.
-- Returns an `AT.Assembly` containing the parsed assembly expression.
parseAssembly :: PU.Parser AT.Expr
parseAssembly = do
  srcLoc <- PU.parseSrcLoc
  AT.Assembly srcLoc <$> (PU.symbol "__asm__" *> PA.parseAsm parseExpr PT.parseType)
