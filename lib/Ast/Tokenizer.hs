module Ast.Tokenizer (Token (..), Separator (..), tokenize) where

import Ast.Types (Literal (..))
import Control.Applicative (Alternative (some), (<|>))
import qualified Data.Void as V
import GHC.Base (Alternative (empty))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

type Parser = M.Parsec V.Void String

data Separator = OpenParen | CloseParen deriving (Show, Eq, Ord)

data Token = TLiteral Literal | TSeparator Separator deriving (Show, Eq, Ord)

tokenize :: String -> Either String [Token]
tokenize input = case M.parse (some token <* M.eof) "" input of
  Left err -> Left (M.errorBundlePretty err)
  Right tokens -> Right tokens

token :: Parser Token
token = lexeme $ M.choice [separator, boolean, number, symbol]

sc :: Parser ()
sc = ML.space MC.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = ML.lexeme sc

separator :: Parser Token
separator = TSeparator OpenParen <$ MC.char '(' <|> TSeparator CloseParen <$ MC.char ')'

boolean :: Parser Token
boolean = TLiteral (LBool True) <$ MC.string "#t" <|> TLiteral (LBool False) <$ MC.string "#f"

number :: Parser Token
number = M.try $ TLiteral . LInt <$> ML.signed (pure ()) ML.decimal

symbol :: Parser Token
symbol = TLiteral . LSymbol <$> some (MC.alphaNumChar <|> MC.symbolChar <|> M.oneOf "+-*_")
