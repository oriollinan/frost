module Ast.Parser (parse) where

import Ast.Types (AST (..), Expr (..), Literal (..), Operation (..))
import Control.Applicative (Alternative (..))
import qualified Data.Void as V
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

type Parser = M.Parsec V.Void String

parse :: String -> Either String AST
parse input = case M.parse parseProgram "" input of
  Left err -> Left (M.errorBundlePretty err)
  Right tokens -> Right tokens

parseProgram :: Parser AST
parseProgram = do
  ast <- parseAst
  M.eof
  return ast

parseAst :: Parser AST
parseAst =
  lexeme $
    let parsers = [parseDefine, parseLambda, parseIf, parseCall, parseOp, parseVar, parseLit]
        triedParsers = map M.try (init parsers) ++ [last parsers]
     in M.choice triedParsers

parseLit :: Parser AST
parseLit = AST . (: []) . Lit <$> M.choice [parseInt, parseBool]

parseInt :: Parser Literal
parseInt = LInt <$> ML.signed sc ML.decimal

parseBool :: Parser Literal
parseBool = LBool True <$ MC.string "#t" <|> LBool False <$ MC.string "#f"

parseVar :: Parser AST
parseVar = AST . (: []) . Var <$> some (MC.alphaNumChar <|> MC.symbolChar <|> M.oneOf "+-*_")

parseDefine :: Parser AST
parseDefine = fail ""

parseCall :: Parser AST
parseCall = fail ""

parseLambda :: Parser AST
parseLambda = fail ""

parseIf :: Parser AST
parseIf = do
  _ <- MC.string "if" <* sc
  e1 <- parseAst <* sc
  e2 <- parseAst <* sc
  e3 <- parseAst
  return $ AST . (: []) $ If e1 e2 e3

parseOp :: Parser AST
parseOp = do
  op <- parseOpeartor <* sc
  e1 <- parseAst <* sc
  e2 <- parseAst
  return $ AST . (: []) $ Op op e1 e2

parseOpeartor :: Parser Operation
parseOpeartor = M.choice $ (\(o, c) -> c <$ MC.string o) <$> ops

ops :: [(String, Operation)]
ops = [("+", Add), ("-", Sub), ("*", Mult), ("div", Div), (">", Gt), ("<", Lt), (">=", Gte), ("<=", Lte), ("==", Equal), ("&&", And), ("||", Or)]

sc :: Parser ()
sc = ML.space MC.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = ML.lexeme sc
