module Ast.Parser.Type where

import qualified Ast.Parser.Env as E
import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

-- | Parse a general type. This function combines multiple specific type parsers.
-- It tries to match typedefs, structs, unions, functions, mutable types, pointers, and base types.
parseType :: AU.Parser AT.Type
parseType = AU.triedChoice [functionType, mutableType, arrayType, pointerType, customIntType, baseType, customType]

-- | A list of predefined base types along with their associated keywords.
-- These include basic types such as int, float, double, char, bool, and void.
baseTypes :: [(String, AT.Type)]
baseTypes =
  [ ("int", AT.TInt 32),
    ("float", AT.TFloat),
    ("double", AT.TDouble),
    ("char", AT.TChar),
    ("bool", AT.TBoolean),
    ("void", AT.TVoid)
  ]

-- | Parses a user-defined integer size.
-- Example: "int128" would result in AT.TInt 128.
customIntType :: AU.Parser AT.Type
customIntType = do
  _ <- AU.symbol "int"
  AT.TInt <$> ML.decimal

-- | Parses a base type by matching one of the predefined base type keywords.
-- Example: "int" or "bool".
baseType :: AU.Parser AT.Type
baseType = M.choice $ (\(kw, ty) -> ty <$ AU.symbol kw) <$> baseTypes

-- | Parses a pointer type.
-- A pointer type is denoted by a '*' followed by another type.
-- Example: "*int" results in a pointer to an integer.
pointerType :: AU.Parser AT.Type
pointerType = AT.TPointer <$> (MC.char '*' *> parseType)

-- | Parses a mutable type.
-- A mutable type is prefixed by the keyword "mut" followed by the type.
-- Example: "mut int" indicates a mutable integer type.
mutableType :: AU.Parser AT.Type
mutableType = AT.TMutable <$> (AU.symbol "mut" *> parseType)

-- | Parses an array type.
-- An array type is denoted by square brackets "[]" followed by the type.
-- Example: "[]int" results in an array of integers.
arrayType :: AU.Parser AT.Type
arrayType = do
  size <- M.between (MC.char '[') (MC.char ']') $ M.optional ML.decimal
  elemType <- parseType
  return $ AT.TArray elemType size

-- | Parses a function type.
-- A function type is defined by its parameter types enclosed in parentheses, followed by "->", and the return type also enclosed in parentheses.
-- Example: "(int) -> (float)" or "(int int) -> (void)".
-- TODO: find a way to do it without the parenthesis and avoid the infinite loop of parseType
functionType :: AU.Parser AT.Type
functionType = do
  paramTypes <- M.between (AU.symbol "(") (AU.symbol ")") $ M.some (AU.lexeme parseType)
  _ <- AU.sc *> AU.symbol "->"
  returnType <- M.between (AU.symbol "(") (AU.symbol ")") parseType
  return $ AT.TFunction {AT.returnType = returnType, AT.paramTypes = paramTypes, AT.isVariadic = False}

customType :: AU.Parser AT.Type
customType = do
  name <- AU.identifier
  env <- S.get
  case E.lookupType name env of
    Just ty -> return ty
    Nothing -> fail $ "Unknown type: " ++ name
