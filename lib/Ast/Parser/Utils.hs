module Ast.Parser.Utils where

import qualified Ast.Parser.Env as E
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

-- | A type alias for the parser, based on `Parsec` with `Void` error type and `String` input.
type Parser = M.ParsecT ParseErrorCustom String (S.State E.Env)

data ParseErrorCustom
  = UnknownType String
  | UndefinedVar String
  deriving (Show, Ord, Eq)

instance M.ShowErrorComponent ParseErrorCustom where
  showErrorComponent (UnknownType n) =
    "Unknown type: type \"" ++ n ++ "\" does not exist"
  showErrorComponent (UndefinedVar n) =
    "Undefined Variable: variable \"" ++ n ++ "\" is not defined"

-- | Skips whitespace and comments (starting with `%`). Ensures proper handling of spacing in parsers.
sc :: Parser ()
sc = ML.space MC.space1 (ML.skipLineComment "%") M.empty

-- | Wraps a parser to consume trailing whitespace, returning the result of the inner parser.
lexeme :: Parser a -> Parser a
lexeme = ML.lexeme sc

-- | Parses a specific symbol (e.g., "+", "-") while skipping trailing whitespace.
symbol :: String -> Parser String
symbol = ML.symbol sc

-- | Tries each parser in the list sequentially, allowing backtracking for all but the last parser.
triedChoice :: [Parser a] -> Parser a
triedChoice ps =
  let triedPs = map M.try (init ps) ++ [last ps]
   in M.choice triedPs

-- | An identifier in our language syntax
identifier :: Parser String
identifier = lexeme ((:) <$> MC.letterChar <*> M.many MC.alphaNumChar)
