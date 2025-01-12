module Ast.Parser.Type where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Data.Maybe as M
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

-- | Parse a general type. This function combines multiple specific type parsers.
-- It tries to match typedefs, structs, unions, functions, mutable types, pointers, and base types.
parseType :: PU.Parser AT.Type
parseType = M.choice [M.try functionType, parseTermType]

parseTermType :: PU.Parser AT.Type
parseTermType =
  M.choice
    [ M.try customIntType,
      baseType,
      mutableType,
      arrayType,
      pointerType,
      customType
    ]

-- | A list of predefined base types along with their associated keywords.
-- These include basic types such as int, float, double, char, bool, and void.
baseTypes :: [(String, AT.Type)]
baseTypes =
  [ ("int", AT.TInt 32),
    ("float", AT.TFloat),
    ("double", AT.TDouble),
    ("char", AT.TChar),
    ("bool", AT.TBoolean),
    ("never", AT.TVoid),
    ("byte", AT.TInt 8)
  ]

-- | Parses a user-defined integer size.
-- Example: "int128" would result in AT.TInt 128.
customIntType :: PU.Parser AT.Type
customIntType = do
  _ <- PU.symbol "int"
  AT.TInt <$> PU.lexeme ML.decimal

-- | Parses a base type by matching one of the predefined base type keywords.
-- Example: "int" or "bool".
baseType :: PU.Parser AT.Type
baseType = M.choice $ (\(kw, ty) -> ty <$ PU.symbol kw) <$> baseTypes

-- | Parses a pointer type.
-- A pointer type is denoted by a '*' followed by another type.
-- Example: "*int" results in a pointer to an integer.
pointerType :: PU.Parser AT.Type
pointerType = do
  _ <- MC.char '*'
  AT.TPointer <$> parseTermType

-- | Parses a mutable type.
-- A mutable type is prefixed by the keyword "mut" followed by the type.
-- Example: "mut int" indicates a mutable integer type.
mutableType :: PU.Parser AT.Type
mutableType = AT.TMutable <$> (PU.symbol "mut" *> parseType)

-- | Parses an array type.
-- An array type is denoted by square brackets "[]" followed by the type.
-- Example: "[]int" results in an array of integers.
arrayType :: PU.Parser AT.Type
arrayType = do
  size <- M.between (MC.char '[') (MC.char ']') $ M.optional ML.decimal
  elemType <- parseType
  return $ AT.TArray elemType size

-- | Parses a function type.
-- A function type is defined by its parameter types, followed by "->", and the return type also enclosed in parentheses.
-- Example: "int -> float" or "int int -> void".
-- TODO: find a way to do it without the parenthesis and avoid the infinite loop of parseType
functionType :: PU.Parser AT.Type
functionType = do
  paramTypes <- M.some $ PU.lexeme $ functionParser M.<|> parseTermType
  variadic <- M.fromMaybe False <$> M.optional (True <$ PU.symbol "...")
  returnType <- PU.symbol "->" *> PU.lexeme (functionParser M.<|> parseTermType)
  return $ AT.TFunction {AT.returnType = returnType, AT.paramTypes = paramTypes, AT.isVariadic = variadic}
  where
    functionParser = M.between (PU.symbol "(") (PU.symbol ")") functionType

customType :: PU.Parser AT.Type
customType = do
  name <- PU.identifier
  env <- S.get
  case PS.lookupType name env of
    Just ty -> return ty
    Nothing -> M.customFailure $ PU.UnknownType name
