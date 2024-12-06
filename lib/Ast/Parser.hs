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
  sc
  ast <- parseAst
  M.eof
  return ast

parseAst :: Parser AST
parseAst = lexeme $ triedChoice [M.try parseList, parseAtom]

parseList :: Parser AST
parseList = List <$> M.between (lexeme $ MC.char '(') (lexeme $ MC.char ')') (M.many parseAst)

parseAtom :: Parser AST
parseAtom = Atom <$> triedChoice [parseDefine, parseLambda, parseIf, parseCall, parseOp, parseLit, parseVar]

parseLit :: Parser Expr
parseLit = Lit <$> M.choice [parseInt, parseBool]

parseInt :: Parser Literal
parseInt = LInt <$> ML.signed (pure ()) ML.decimal

parseBool :: Parser Literal
parseBool = LBool True <$ MC.string "#t" <|> LBool False <$ MC.string "#f"

parseVar :: Parser Expr
parseVar = Var <$> some (MC.alphaNumChar <|> M.oneOf "+-*_")

parseDefine :: Parser Expr
parseDefine = do
  _ <- MC.string "define" <* sc
  e1 <- parseAst <* sc
  Define e1 <$> parseAst

parseCall :: Parser Expr
parseCall = fail ""

parseLambda :: Parser Expr
parseLambda = fail ""

parseIf :: Parser Expr
parseIf = do
  _ <- MC.string "if" <* sc
  e1 <- parseAst <* sc
  e2 <- parseAst <* sc
  If e1 e2 <$> parseAst

parseOp :: Parser Expr
parseOp = do
  op <- parseOpeartor <* sc
  e1 <- parseAst <* sc
  Op op e1 <$> parseAst

parseOpeartor :: Parser Operation
parseOpeartor = M.choice $ (\(o, c) -> c <$ MC.string o) <$> ops

ops :: [(String, Operation)]
ops = [("+", Add), ("-", Sub), ("*", Mult), ("div", Div), (">", Gt), ("<", Lt), (">=", Gte), ("<=", Lte), ("==", Equal), ("&&", And), ("||", Or)]

sc :: Parser ()
sc = ML.space MC.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = ML.lexeme sc

triedChoice :: [Parser a] -> Parser a
triedChoice ps =
  let triedPs = map M.try (init ps) ++ [last ps]
   in M.choice triedPs
