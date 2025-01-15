module Ast.Parser.Literal where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
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
parseLiteral = PU.triedChoice [parseArray, parseChar, parseFloat, parseInt, parseBool, parseNull, parseStruct]

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
parseBool = AT.LBool <$> PU.parseBool

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
        <$> M.between (MC.char '\"') (MC.char '\"') (M.many PU.parseStringChar)

    parseLiteralArray =
      AT.LArray
        <$> M.between (PU.symbol "[") (PU.symbol "]") (M.sepBy parseLiteral PU.sc)

-- | Parses a `null` literal.
-- Returns a `Literal` of type `LNull`.
parseNull :: PU.Parser AT.Literal
parseNull = AT.LNull <$ PU.symbol nullSymbol

parseStruct :: PU.Parser AT.Literal
parseStruct = do
  name <- PU.lexeme PU.identifier
  fields <- M.between (PU.symbol "{") (PU.symbol "}") $ M.some parseField
  state <- S.get
  case PS.lookupType name state of
    (Just _) -> return $ AT.LStruct fields
    _ -> M.customFailure $ PU.UnknownType name
  where
    parseField = (,) <$> PU.lexeme PU.identifier <* PU.symbol "=" <*> PU.lexeme parseLiteral
