module Ast.Parser.Utils where

import qualified Ast.Parser.State as PS
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Pos as MP

-- | A type alias for the parser, based on `Parsec` with `Void` error type and `String` input.
type Parser = M.ParsecT ParseErrorCustom String (S.State PS.ParserState)

data ParseErrorCustom
  = UnknownType String
  | UndefinedVar String
  | UndefinedFunction String
  | InvalidFunctionType String AT.Type
  deriving (Show, Ord, Eq)

instance M.ShowErrorComponent ParseErrorCustom where
  showErrorComponent (UnknownType n) =
    "Unknown type: type \"" ++ n ++ "\" does not exist"
  showErrorComponent (UndefinedVar n) =
    "Undefined Variable: variable \"" ++ n ++ "\" is not defined"
  showErrorComponent (UndefinedFunction n) =
    "Undefined Function: function \"" ++ n ++ "\" is not defined"
  showErrorComponent (InvalidFunctionType n t) =
    "Invalid Function Type: function \"" ++ n ++ "\" with type \"" ++ show t ++ "\" is not valid"

-- | Skips whitespace and comments (starting with `%`). Ensures proper handling of spacing in parsers.
sc :: Parser ()
sc = ML.space MC.space1 (ML.skipLineComment "%") $ ML.skipBlockComment "%%" "%%"

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
identifier = lexeme ((:) <$> (MC.letterChar M.<|> M.oneOf "$") <*> M.many (MC.alphaNumChar M.<|> M.oneOf "$"))

-- | Gets the SrcLoc
parseSrcLoc :: Parser AT.SrcLoc
parseSrcLoc = do
  (MP.SourcePos {MP.sourceName = _sourceName, MP.sourceLine = _sourceLine, MP.sourceColumn = _sourceColumn}) <- M.getSourcePos
  return $ AT.SrcLoc {AT.srcFile = _sourceName, AT.srcLine = MP.unPos _sourceLine, AT.srcCol = MP.unPos _sourceColumn}
