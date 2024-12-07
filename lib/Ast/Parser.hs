module Ast.Parser (parse) where

import Ast.Types (AST (..), Expr (..), Literal (..), Operation (..))
import Control.Applicative (Alternative (..))
import qualified Control.Monad as M
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

type Parser = M.Parsec ParseErrorCustom String

data ParseErrorCustom
  = InvalidDefineExpression Expr
  | InvalidArgsForDefine Expr
  | InvalidLambdaExpression Expr
  | InvalidArgsForLambda Expr
  | ReservedKeywordUsed String
  deriving (Show, Ord, Eq)

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

parse :: String -> Either String AST
parse input = case M.parse parseProgram "" input of
  Left err -> Left (M.errorBundlePretty err)
  Right tokens -> Right tokens

parseProgram :: Parser AST
parseProgram = do
  sc
  ast <- parseAst
  M.eof
  return ast

parseAst :: Parser AST
parseAst = lexeme $ AST <$> M.many parseExpr

parseExpr :: Parser Expr
parseExpr = triedChoice [parseAtom, parseList]

parseAtom :: Parser Expr
parseAtom = triedChoice [list parseDefine, list parseLambda, list parseIf, list parseOp, list parseCall, parseLit, parseVar]

parseList :: Parser Expr
parseList = Seq <$> list (M.many parseExpr)

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

-- parseDefine = do
--   _ <- symbol "define"
--   M.choice [parseDefineVar, parseDefineFunc]

-- parseDefineVar :: Parser Expr
-- parseDefineVar = do
--   name <- lexeme parseVarName
--   Define name <$> parseExpr

-- parseDefineFunc :: Parser Expr
-- parseDefineFunc = do
--   call <- lexeme $ M.between (symbol "(") (symbol ")") parseCall
--   e <- M.between (symbol "(") (symbol ")") parseExpr
--   case call of
--     (Call (Var name) es) -> case extractVarNames es of
--       (Just args) -> return $ Define name $ Lambda args e
--       _ -> M.customFailure $ InvalidArgsForDefine es
--     _ -> M.customFailure $ InvalidDefineExpression call

-- parseDefine = do
-- _ <- symbol "define"
-- e <- lexeme parseExpr
-- value <- parseExpr
-- case e of
--   (Var name) -> return $ Define name value
--   (Call (Var name) es) -> case extractVarNames es of
--     (Just args) -> return $ Define name $ Lambda args value
--     _ -> M.customFailure $ InvalidArgsForDefine es
--   _ -> M.customFailure $ InvalidDefineExpression e

parseLambda :: Parser Expr
parseLambda = do
  _ <- symbol "lambda"
  e <- parseExpr
  value <- parseExpr
  case e of
    (Seq es) -> case extractVarNames es of
      (Just params) -> return $ Lambda params value
      _ -> M.customFailure $ InvalidArgsForLambda $ Seq es
    _ -> M.customFailure $ InvalidLambdaExpression e

-- parseLambda = do
--   _ <- symbol "lambda"
--   params <- M.between (symbol "(") (symbol ")") (M.many parseVarName)
--   Lambda params <$> parseExpr

extractVarNames :: [Expr] -> Maybe [String]
extractVarNames = mapM extractVarName
  where
    extractVarName :: Expr -> Maybe String
    extractVarName (Var name) = Just name
    extractVarName _ = Nothing

parseIf :: Parser Expr
parseIf = do
  _ <- symbol "if"
  e1 <- lexeme parseExpr
  e2 <- lexeme parseExpr
  If e1 e2 <$> parseExpr

parseOp :: Parser Expr
parseOp = do
  op <- lexeme parseOpeartor
  e1 <- lexeme parseExpr
  Op op e1 <$> parseExpr

parseOpeartor :: Parser Operation
parseOpeartor = M.choice $ (\(o, c) -> c <$ symbol o) <$> ops

parseCall :: Parser Expr
parseCall = do
  name <- lexeme parseVarName
  M.when (name `elem` keywords) $ M.customFailure $ ReservedKeywordUsed name
  args <- M.some $ parseExpr <* M.optional sc
  return $ Call (Var name) $ Seq args

parseLit :: Parser Expr
parseLit = Lit <$> M.choice [parseInt, parseBool]

parseInt :: Parser Literal
parseInt = LInt <$> ML.signed (pure ()) ML.decimal

parseBool :: Parser Literal
parseBool = LBool True <$ symbol "#t" <|> LBool False <$ symbol "#f"

parseVar :: Parser Expr
parseVar = Var <$> parseVarName

parseVarName :: Parser String
parseVarName = M.some (MC.alphaNumChar <|> M.oneOf "_")

sc :: Parser ()
sc = ML.space MC.space1 empty empty

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
