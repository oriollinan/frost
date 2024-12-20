module Ast.Parser.Literal where

import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

trueSymbol :: String
trueSymbol = "true"

falseSymbol :: String
falseSymbol = "false"

nullSymbol :: String
nullSymbol = "null"

parseLiteral :: AU.Parser AT.Literal
parseLiteral = AU.triedChoice [parseArray, parseChar, parseFloat, parseInt, parseBool, parseNull]

-- | Parses an integer literal, supporting signed values.
-- Returns a `Literal` of type `LInt`.
parseInt :: AU.Parser AT.Literal
parseInt = AT.LInt <$> ML.signed (pure ()) ML.decimal

-- | Parses a floating-point literal.
-- Returns a `Literal` of type `LFloat`.
parseFloat :: AU.Parser AT.Literal
parseFloat = AT.LFloat <$> ML.signed (pure ()) ML.float

-- | Parses a boolean literal (`true` or `false`).
-- Returns a `Literal` of type `LBool`.
parseBool :: AU.Parser AT.Literal
parseBool = AT.LBool True <$ AU.symbol trueSymbol <|> AT.LBool False <$ AU.symbol falseSymbol

-- | Parses a character literal (e.g., 'a').
-- Returns a `Literal` of type `LChar`.
parseChar :: AU.Parser AT.Literal
parseChar = AT.LChar <$> M.between (MC.char '\'') (MC.char '\'') M.anySingle

-- | Parses an array of literals.
-- Supports string literals as arrays of characters or standard arrays of literals.
-- Returns a `Literal` of type `LArray`.
parseArray :: AU.Parser AT.Literal
parseArray =
  M.choice
    [ AT.LArray . map AT.LChar <$> M.between (MC.char '\"') (MC.char '\"') (M.many (M.noneOf ['"'])),
      AT.LArray <$> M.between (AU.symbol "[") (AU.symbol "]") (M.sepBy parseLiteral (AU.symbol ","))
    ]

-- | Parses a `null` literal.
-- Returns a `Literal` of type `LNull`.
parseNull :: AU.Parser AT.Literal
parseNull = AT.LNull <$ AU.symbol nullSymbol
