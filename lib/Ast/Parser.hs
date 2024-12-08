module Ast.Parser (parse) where

import Ast.Types (AST (..), Expr (..), Literal (..), Operation (..))
import Control.Applicative (Alternative (..))
import qualified Control.Monad as M
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

type Parser = M.Parsec ParseErrorCustom String

-- | Custom error type for the parser, representing specific cases of invalid syntax.
data ParseErrorCustom
  = InvalidDefineExpression Expr
  | InvalidArgsForDefine Expr
  | InvalidLambdaExpression Expr
  | InvalidArgsForLambda Expr
  | ReservedKeywordUsed String
  deriving (Show, Ord, Eq)

-- | Implements a custom error message component for the parser.
instance M.ShowErrorComponent ParseErrorCustom where
  showErrorComponent (InvalidDefineExpression e) =
    "Invalid define expression: expected a variable or a function definition, but got: " ++ show e
  showErrorComponent (InvalidArgsForDefine e) =
    "Invalid arguments in define: expected all arguments to be variables, but got: " ++ show e
  showErrorComponent (InvalidLambdaExpression e) =
    "Invalid lambda expression: expected a function definition, but got: " ++ show e
  showErrorComponent (InvalidArgsForLambda e) =
    "Invalid arguments in lambda: expected all arguments to be variables, but got: " ++ show e
  showErrorComponent (ReservedKeywordUsed kw) =
    "Reserved keyword used as function name: \"" ++ kw ++ "\""

ops :: [(String, Operation)]
ops = [("+", Add), ("-", Sub), ("*", Mult), ("div", Div), (">", Gt), ("<", Lt), (">=", Gte), ("<=", Lte), ("==", Equal), ("&&", And), ("||", Or)]

keywords :: [String]
keywords = ["define", "lambda", "if"] ++ map fst ops

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a String as a parameter and returns either an AST or an error message.
parse :: String -> Either String AST
parse input = case M.parse parseProgram "" input of
  Left err -> Left (M.errorBundlePretty err)
  Right tokens -> Right tokens

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
-- Returns an `Expr` of type `Seq` that wraps the parsed expressions.
parseList :: Parser Expr
parseList = Seq <$> list (M.many parseExpr)

-- | Parses a `define` expression for variable or function definition.
-- Returns an `Expr` representing the parsed `define` construct.
parseDefine :: Parser Expr
parseDefine = do
  _ <- symbol "define"
  e <- lexeme parseExpr
  value <- parseExpr
  case e of
    (Var name) -> return $ Define name value
    (Call (Var name) (Seq es)) -> case extractVarNames es of
      (Just args) -> return $ Define name $ Lambda args value
      _ -> M.customFailure $ InvalidArgsForDefine $ Seq es
    _ -> M.customFailure $ InvalidDefineExpression e

-- | Parses a `lambda` expression for defining functions.
-- Returns an `Expr` representing the parsed `lambda` construct.
parseLambda :: Parser Expr
parseLambda = do
  _ <- symbol "lambda"
  e <- parseExpr
  value <- parseExpr
  case e of
    (Seq es) -> case extractVarNames es of
      (Just params) -> return $ Lambda params value
      _ -> M.customFailure $ InvalidArgsForLambda $ Seq es
    (Call (Var n) (Seq es)) -> case extractVarNames es of
      (Just params) -> return $ Lambda (n : params) value
      _ -> M.customFailure $ InvalidArgsForLambda $ Seq $ Var n : es
    _ -> M.customFailure $ InvalidLambdaExpression e

extractVarNames :: [Expr] -> Maybe [String]
extractVarNames = mapM extractVarName
  where
    extractVarName :: Expr -> Maybe String
    extractVarName (Var name) = Just name
    extractVarName _ = Nothing

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
  M.when (name `elem` keywords) $ M.customFailure $ ReservedKeywordUsed name
  args <- M.some $ parseExpr <* M.optional sc
  return $ Call (Var name) $ Seq args

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
parseVar = Var <$> parseVarName

-- | Parses a variable name consisting of alphanumeric characters and underscores.
-- Returns a `String` representing the variable name.
parseVarName :: Parser String
parseVarName = M.some (MC.alphaNumChar <|> M.oneOf "_")

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
