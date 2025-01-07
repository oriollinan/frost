module Ast.Parser.Literal where

import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
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

parseLiteral :: PU.Parser AT.Literal
parseLiteral = PU.triedChoice [parseArray, parseChar, parseFloat, parseInt, parseBool, parseNull]

-- | Parses an integer literal, supporting signed values.
-- Returns a `Literal` of type `LInt`.
parseInt :: PU.Parser AT.Literal
parseInt = AT.LInt <$> ML.signed (pure ()) ML.decimal

-- | Parses a floating-point literal.
-- Returns a `Literal` of type `LFloat`.
parseFloat :: PU.Parser AT.Literal
parseFloat = AT.LFloat <$> ML.signed (pure ()) ML.float

-- | Parses a boolean literal (`true` or `false`).
-- Returns a `Literal` of type `LBool`.
parseBool :: PU.Parser AT.Literal
parseBool = AT.LBool True <$ PU.symbol trueSymbol <|> AT.LBool False <$ PU.symbol falseSymbol

-- | Parses a character literal (e.g., 'a').
-- Returns a `Literal` of type `LChar`.
parseChar :: PU.Parser AT.Literal
parseChar = AT.LChar <$> M.between (MC.char '\'') (MC.char '\'') M.anySingle

-- | Parses an array of literals.
-- Supports string literals as arrays of characters or standard arrays of literals.
-- Returns a `Literal` of type `LArray`.
parseArray :: PU.Parser AT.Literal
parseArray =
  M.choice
    [ AT.LArray . map AT.LChar <$> M.between (MC.char '\"') (MC.char '\"') (M.many (M.noneOf ['"'])),
      AT.LArray <$> M.between (PU.symbol "[") (PU.symbol "]") (M.sepBy parseLiteral (PU.symbol ","))
    ]

-- | Parses a `null` literal.
-- Returns a `Literal` of type `LNull`.
parseNull :: PU.Parser AT.Literal
parseNull = AT.LNull <$ PU.symbol nullSymbol
