module Ast.Parser (parse, ParseErrorCustom (..)) where

import qualified Ast.Env as E
import Ast.Types (AST (..), Expr (..), Literal (..), Operation (..))
import Control.Applicative (Alternative (..))
import qualified Control.Monad as CM
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

type Parser = M.ParsecT ParseErrorCustom String (S.State E.Env)

-- | Custom error type for the parser, representing specific cases of invalid syntax.
data ParseErrorCustom
  = UndefinedLambdaReference String
  | ReservedKeywordUsed String
  | UndefinedVarReference String
  | InvalidVarName String
  deriving (Show, Ord, Eq)

-- | Implements a custom error message component for the parser.
instance M.ShowErrorComponent ParseErrorCustom where
  showErrorComponent (UndefinedLambdaReference n) =
    "Undefined lambda referenced: expected lambda \"" ++ n ++ "\" to be defined"
  showErrorComponent (ReservedKeywordUsed kw) =
    "Reserved keyword used as function name: \"" ++ kw ++ "\""
  showErrorComponent (UndefinedVarReference n) =
    "Undefined var referenced: expected var \"" ++ n ++ "\" to be defined"
  showErrorComponent (InvalidVarName n) =
    "Invalid var name: \"" ++ n ++ "\" is not valid"

ops :: [(String, Operation)]
ops = [("+", Add), ("-", Sub), ("*", Mult), ("div", Div), ("mod", Mod), (">=", Gte), ("<=", Lte), (">", Gt), ("<", Lt), ("==", Equal), ("/=", Ne), ("&&", And), ("||", Or)]

keywords :: [String]
keywords = ["define", "lambda", "if"] ++ map fst ops

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a filename `String`, and an input `String` as a parameter and returns either an AST or an error message.
parse :: String -> String -> Either String AST
parse filename input = case S.runState (M.runParserT parseProgram filename input) E.emptyEnv of
  (Left err, _) -> Left (M.errorBundlePretty err)
  (Right tokens, _) -> Right tokens

-- | Parses the top-level structure of a program.
-- Returns an `AST` object containing a list of parsed expressions.
parseProgram :: Parser AST
parseProgram = do
  sc
  ast <- parseAst
  M.eof
  return ast

-- | Parses the body of a program into an `AST`.
-- Returns an `AST` that wraps multiple parsed expressions.
parseAst :: Parser AST
parseAst = lexeme $ AST <$> M.many parseExpr

-- | Parses an individual expression.
-- Returns an `Expr`, which could be an atom or list.
parseExpr :: Parser Expr
parseExpr = triedChoice [parseAtom, parseList]

-- | Parses atomic expressions like literals, variables, or predefined constructs.
-- Returns an `Expr` representing the parsed atomic expression.
parseAtom :: Parser Expr
parseAtom = triedChoice [list parseDefine, list parseLambda, list parseIf, list parseOp, list parseCall, parseLit, parseVar]

-- | Parses a sequence of expressions enclosed in parentheses.
-- If the first element is callable, it treats the list as a function call.
-- Otherwise, it creates a sequence of expressions (`Seq`).
parseList :: Parser Expr
parseList = do
  exprs <- list (M.many parseExpr)
  case exprs of
    (fn : args) | isCallable fn -> return $ Call fn (Seq args)
    _ -> return $ Seq exprs

-- | Determines whether an expression can be treated as callable.
isCallable :: Expr -> Bool
isCallable (Var _) = True
isCallable (Lambda _ _) = True
isCallable _ = False

-- | Parses a `define` expression for variable or function definition.
-- Returns an `Expr` representing the parsed `define` construct.
parseDefine :: Parser Expr
parseDefine = do
  _ <- symbol "define"
  M.choice [parseVariableDefine, parseFunctionDefine]

parseVariableDefine :: Parser Expr
parseVariableDefine = do
  name <- lexeme parseVarName
  value <- parseExpr
  case value of
    (Lambda _ _) -> S.modify $ E.insertFn name value
    _ -> S.modify $ E.insertVar name value
  return $ Define name value

parseFunctionDefine :: Parser Expr
parseFunctionDefine = do
  (name, args) <- list $ do
    name <- lexeme parseVarName
    args <- M.many $ parseVarName <* M.optional sc
    return (name, args)
  mapM_ (\arg -> S.modify $ E.insertVar arg $ Var arg) args
  value <- parseExpr
  let lambda = Lambda args value
  S.modify $ E.insertFn name lambda
  return $ Define name lambda

-- | Parses a `lambda` expression for defining functions.
-- Returns an `Expr` representing the parsed `lambda` construct.
parseLambda :: Parser Expr
parseLambda = do
  _ <- symbol "lambda"
  params <- list $ M.many $ parseVarName <* M.optional sc
  mapM_ (\p -> S.modify $ E.insertVar p $ Var p) params
  Lambda params <$> parseExpr

-- | Parses an `if` expression with a condition, true branch, and false branch.
-- Returns an `Expr` of type `If`.
parseIf :: Parser Expr
parseIf = do
  _ <- symbol "if"
  e1 <- lexeme parseExpr
  e2 <- lexeme parseExpr
  If e1 e2 <$> parseExpr

-- | Parses a binary operation with two operands.
-- Returns an `Expr` of type `Op`.
parseOp :: Parser Expr
parseOp = do
  op <- lexeme parseOpeartor
  e1 <- lexeme parseExpr
  Op op e1 <$> parseExpr

-- | Parses an operator like `+`, `-`, or `*`.
-- Returns an `Operation` corresponding to the operator.
parseOpeartor :: Parser Operation
parseOpeartor = M.choice $ (\(o, c) -> c <$ symbol o) <$> ops

-- | Parses a function call with a name and arguments.
-- Returns an `Expr` of type `Call`.
parseCall :: Parser Expr
parseCall = do
  name <- lexeme parseVarName
  env <- S.get
  case E.lookupFn name env of
    Just (Lambda _ _) -> do
      args <- M.many $ parseExpr <* M.optional sc
      return $ Call (Var name) $ Seq args
    _ -> M.customFailure $ UndefinedLambdaReference name

-- | Parses a literal value (integer or boolean).
-- Returns an `Expr` of type `Lit`.
parseLit :: Parser Expr
parseLit = Lit <$> M.choice [parseInt, parseBool]

-- | Parses an integer literal, supporting signed values.
-- Returns a `Literal` of type `LInt`.
parseInt :: Parser Literal
parseInt = LInt <$> ML.signed (pure ()) ML.decimal

-- | Parses a boolean literal (`#t` or `#f`).
-- Returns a `Literal` of type `LBool`.
parseBool :: Parser Literal
parseBool = LBool True <$ symbol "#t" <|> LBool False <$ symbol "#f"

-- | Parses a variable expression by matching valid variable names.
-- Returns an `Expr` of type `Var`.
parseVar :: Parser Expr
parseVar = do
  name <- parseVarName
  env <- S.get
  case E.lookupVar name env of
    Just _ -> do
      return $ Var name
    _ -> M.customFailure $ UndefinedVarReference name

-- | Parses a variable name consisting of alphanumeric characters and underscores.
-- Returns a `String` representing the variable name.
parseVarName :: Parser String
parseVarName = do
  name <- M.some (MC.alphaNumChar <|> M.oneOf "_$")
  CM.when (name `elem` keywords) $ M.customFailure $ ReservedKeywordUsed name
  CM.when (all (`elem` ['0' .. '9']) name) $ M.customFailure $ InvalidVarName name
  return name

-- | Skips whitespace and comments during parsing.
-- Used to ensure parsers handle spacing correctly.
sc :: Parser ()
sc = ML.space MC.space1 empty empty

-- | Wraps a parser to handle leading and trailing whitespace.
-- Returns the result of the inner parser.
lexeme :: Parser a -> Parser a
lexeme = ML.lexeme sc

symbol :: String -> Parser String
symbol = ML.symbol sc

triedChoice :: [Parser a] -> Parser a
triedChoice ps =
  let triedPs = map M.try (init ps) ++ [last ps]
   in M.choice triedPs

list :: Parser a -> Parser a
list = M.between (symbol "(") (symbol ")")
