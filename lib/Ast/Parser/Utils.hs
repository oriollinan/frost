module Ast.Parser.Utils where

import qualified Ast.Parser.State as PS
import qualified Ast.Types as AT
import qualified Control.Monad.Combinators.Expr as CE
import qualified Control.Monad.State as S
import qualified Data.Char as C
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML
import qualified Text.Megaparsec.Pos as MP

-- | A type alias for the parser, based on `Parsec` with `Void` error type and `String` input.
type Parser = M.ParsecT ParseErrorCustom String (S.StateT PS.ParserState IO)

data ParseErrorCustom
  = UnknownType String
  | InvalidFunctionType String AT.Type
  | InvalidDefer AT.Expr
  deriving (Show, Ord, Eq)

instance M.ShowErrorComponent ParseErrorCustom where
  showErrorComponent (UnknownType n) =
    "Unknown type: type \"" ++ n ++ "\" does not exist"
  showErrorComponent (InvalidFunctionType n t) =
    "Invalid Function Type: function \"" ++ n ++ "\" with type \"" ++ show t ++ "\" is not valid"
  showErrorComponent (InvalidDefer e) =
    "Invalid Defer: defer \"" ++ show e ++ "\" is not valid "

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
identifier = lexeme ((:) <$> (MC.letterChar M.<|> M.oneOf "$_") <*> M.many (MC.alphaNumChar M.<|> M.oneOf "$_"))

-- | Gets the SrcLoc
parseSrcLoc :: Parser AT.SrcLoc
parseSrcLoc = do
  (MP.SourcePos {MP.sourceName = _sourceName, MP.sourceLine = _sourceLine, MP.sourceColumn = _sourceColumn}) <- M.getSourcePos
  return $ AT.SrcLoc {AT.srcFile = _sourceName, AT.srcLine = MP.unPos _sourceLine, AT.srcCol = MP.unPos _sourceColumn}

prefix :: String -> (AT.SrcLoc -> AT.Expr -> AT.Expr) -> CE.Operator Parser AT.Expr
prefix name f = CE.Prefix (f <$> (parseSrcLoc <* symbol name))

postfix :: String -> (AT.SrcLoc -> AT.Expr -> AT.Expr) -> CE.Operator Parser AT.Expr
postfix name f = CE.Postfix (f <$> (parseSrcLoc <* symbol name))

-- | Helper functions to define operators
binary :: String -> (AT.SrcLoc -> AT.Expr -> AT.Expr -> AT.Expr) -> CE.Operator Parser AT.Expr
binary name f = CE.InfixL (f <$> (parseSrcLoc <* symbol name))

parseBool :: Parser Bool
parseBool = True <$ MC.string "true" M.<|> False <$ MC.string "false"

parseStringChar :: Parser Char
parseStringChar =
  M.choice
    [ parseEscapeSequence,
      M.noneOf ['"', '\\']
    ]

parseEscapeSequence :: Parser Char
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

parseHexEscape :: Parser Char
parseHexEscape = do
  _ <- MC.char 'x'
  digits <- M.count 2 hexDigit
  return $ C.chr $ read ("0x" ++ digits)

parseOctalEscape :: Parser Char
parseOctalEscape = do
  digits <- M.count 3 octalDigit
  return $ C.chr $ read ("0o" ++ digits)

hexDigit :: Parser Char
hexDigit = M.oneOf $ ['0' .. '9'] ++ ['a' .. 'f'] ++ ['A' .. 'F']

octalDigit :: Parser Char
octalDigit = M.oneOf ['0' .. '7']

normalizeLoc :: AT.SrcLoc
normalizeLoc = AT.SrcLoc "" 0 0

normalizeExpr :: AT.Expr -> AT.Expr
normalizeExpr (AT.Lit _ lit) = AT.Lit normalizeLoc lit
normalizeExpr (AT.Var _ name t) = AT.Var normalizeLoc name t
normalizeExpr (AT.Function _ name t params body) = AT.Function normalizeLoc name t params (normalizeExpr body)
normalizeExpr (AT.Declaration _ name t initVal) = AT.Declaration normalizeLoc name t (fmap normalizeExpr initVal)
normalizeExpr (AT.Assignment _ target value) = AT.Assignment normalizeLoc (normalizeExpr target) (normalizeExpr value)
normalizeExpr (AT.Call _ func args) = AT.Call normalizeLoc (normalizeExpr func) (map normalizeExpr args)
normalizeExpr (AT.If _ cond thenBranch elseBranch) = AT.If normalizeLoc (normalizeExpr cond) (normalizeExpr thenBranch) (fmap normalizeExpr elseBranch)
normalizeExpr (AT.Block exprs) = AT.Block (map normalizeExpr exprs)
normalizeExpr (AT.Return _ value) = AT.Return normalizeLoc (fmap normalizeExpr value)
normalizeExpr (AT.Op _ op e1 e2) = AT.Op normalizeLoc op (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.UnaryOp _ op e) = AT.UnaryOp normalizeLoc op (normalizeExpr e)
normalizeExpr (AT.From _ s e r v b) = AT.From normalizeLoc (normalizeExpr s) (normalizeExpr e) (normalizeExpr r) (normalizeExpr v) (normalizeExpr b)
normalizeExpr (AT.While _ c b) = AT.While normalizeLoc (normalizeExpr c) (normalizeExpr b)
normalizeExpr (AT.Continue _) = AT.Continue normalizeLoc
normalizeExpr (AT.Break _) = AT.Break normalizeLoc
normalizeExpr (AT.StructAccess _ e1 e2) = AT.StructAccess normalizeLoc (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.ArrayAccess _ e1 e2) = AT.ArrayAccess normalizeLoc (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.Cast _ t e) = AT.Cast normalizeLoc t (normalizeExpr e)
normalizeExpr (AT.ForeignFunction _ n t) = AT.ForeignFunction normalizeLoc n t
normalizeExpr (AT.Assembly _ a) = AT.Assembly normalizeLoc a
