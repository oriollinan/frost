module Ast.Parser.Literal where

import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Data.Char as C
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
parseFloat =
  AT.LFloat
    <$> ML.signed
      (pure ())
      ( do
          wholePart <- ML.decimal :: (PU.Parser Integer)
          fractionalPart <- MC.char ',' *> M.some MC.digitChar
          let fractional = read ("0." ++ fractionalPart) :: Double
          let value = fromIntegral wholePart + fractional
          return value
      )

-- | Parses a boolean literal (`true` or `false`).
-- Returns a `Literal` of type `LBool`.
parseBool :: PU.Parser AT.Literal
parseBool = AT.LBool True <$ MC.string trueSymbol M.<|> AT.LBool False <$ MC.string falseSymbol

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
    [ parseStringArray,
      parseLiteralArray
    ]
  where
    parseStringArray =
      AT.LArray . map AT.LChar
        <$> M.between (MC.char '\"') (MC.char '\"') (M.many parseStringChar)

    parseLiteralArray =
      AT.LArray
        <$> M.between (PU.symbol "[") (PU.symbol "]") (M.sepBy parseLiteral PU.sc)

    parseStringChar =
      M.choice
        [ parseEscapeSequence,
          M.noneOf ['"', '\\']
        ]

    parseEscapeSequence =
      MC.char '\\'
        >> M.choice
          [ '\a' <$ MC.char 'a',
            '\b' <$ MC.char 'b',
            '\f' <$ MC.char 'f',
            '\n' <$ MC.char 'n',
            '\r' <$ MC.char 'r',
            '\t' <$ MC.char 't',
            '\v' <$ MC.char 'v',
            '\\' <$ MC.char '\\',
            '\"' <$ MC.char '"',
            '\'' <$ MC.char '\'',
            '\0' <$ MC.char '0',
            parseHexEscape,
            parseOctalEscape
          ]

    parseHexEscape = do
      _ <- MC.char 'x'
      digits <- M.count 2 hexDigit
      return $ C.chr $ read ("0x" ++ digits)

    parseOctalEscape = do
      digits <- M.count 3 octalDigit
      return $ C.chr $ read ("0o" ++ digits)

    hexDigit = M.oneOf $ ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']
    octalDigit = M.oneOf ['0' .. '7']

-- | Parses a `null` literal.
-- Returns a `Literal` of type `LNull`.
parseNull :: PU.Parser AT.Literal
parseNull = AT.LNull <$ PU.symbol nullSymbol
