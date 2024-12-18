module Ast.Parser where

import qualified Ast.Types as AT
import Control.Applicative (Alternative (..))
import Data.Void (Void)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

type Parser = M.Parsec Void String

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a filename `String`, and an input `String` as a parameter and returns either an AST or an error message.
parse :: String -> String -> Either String AT.Program
parse filename input = case M.runParser parseProgram filename input of
  (Left err) -> Left (M.errorBundlePretty err)
  (Right program) -> Right program

-- | Parses the top-level structure of a program.
-- Returns an `AST` object containing a list of parsed expressions.
parseProgram :: Parser AT.Program
parseProgram = fail "undefined"

-- | Skips whitespace and comments during parsing.
-- Used to ensure parsers handle spacing correctly.
sc :: Parser ()
sc = ML.space MC.space1 (ML.skipLineComment ";") empty

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
