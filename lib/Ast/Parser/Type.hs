module Ast.Parser.Type where

import qualified Ast.Parser.Env as E
import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import qualified Control.Monad.State as S
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

-- | Parse a general type. This function combines multiple specific type parsers.
-- It tries to match typedefs, structs, unions, functions, mutable types, pointers, and base types.
parseType :: AU.Parser AT.Type
parseType = AU.triedChoice [structType, unionType, typedefType, functionType, mutableType, arrayType, pointerType, customIntType, baseType, customType]

-- | A list of predefined base types along with their associated keywords.
-- These include basic types such as int, float, double, char, bool, and void.
baseTypes :: [(String, AT.Type)]
baseTypes =
  [ ("int", AT.TInt 0),
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
mutableType = AT.TMutable <$> (AU.symbol "mut" *> AU.sc *> parseType)

-- | Parses an array type.
-- An array type is denoted by square brackets "[]" followed by the type.
-- Example: "[]int" results in an array of integers.
arrayType :: AU.Parser AT.Type
arrayType = do
  size <- M.between (MC.char '[') (MC.char ']') $ M.optional (ML.decimal <* AU.sc)
  elemType <- parseType
  return $ AT.TArray elemType size

-- | Parses a struct type definition.
-- A struct is defined with the "struct" keyword followed by an optional name and a list of fields enclosed in braces.
-- Example: "struct { x -> int, y -> float }".
structType :: AU.Parser AT.Type
structType = do
  name <- AU.identifier
  _ <- AU.symbol "::" <* AU.symbol "struct"
  fields <- M.between (MC.char '{') (MC.char '}') $ M.many (AU.lexeme parseField)
  let newStructType = AT.TStruct {AT.structName = name, AT.fields = fields}
  S.modify (E.insertType name newStructType)
  return newStructType

-- | Parses a union type definition.
-- A union is defined with the "union" keyword followed by an optional name and a list of variants enclosed in braces.
-- Example: "union { data -> *char, error -> int }".
unionType :: AU.Parser AT.Type
unionType = do
  name <- AU.identifier
  _ <- AU.symbol "::" <* AU.symbol "union"
  variants <- M.between (MC.char '{') (MC.char '}') $ M.many (AU.lexeme parseField)
  let newUnionType = AT.TUnion {AT.unionName = name, AT.variants = variants}
  S.modify (E.insertType name newUnionType)
  return newUnionType

-- | Parses a typedef.
-- A typedef associates a new name with an existing type using the "::" syntax.
-- Example: "Vector2i :: Vector".
typedefType :: AU.Parser AT.Type
typedefType = do
  name <- AU.identifier
  _ <- AU.symbol "::"
  baseType <- parseType
  let typedef = AT.TTypedef name baseType
  S.modify (E.insertType name typedef)
  return typedef

-- | Parses a function type.
-- A function type is defined by its parameter types separated by spaces, followed by "->" and the return type.
-- Example: ": Vector2i -> Response" or ": int float -> double".
functionType :: AU.Parser AT.Type
functionType = do
  _ <- AU.symbol ":"
  paramTypes <- M.some (parseType <* AU.sc)
  _ <- AU.symbol "->"
  returnType <- parseType
  return $ AT.TFunction {AT.returnType = returnType, AT.paramTypes = paramTypes, AT.isVariadic = False}

customType :: AU.Parser AT.Type
customType = do
  name <- AU.identifier
  env <- S.get
  case E.lookupType name env of
    Just ty -> return ty
    Nothing -> fail $ "Unknown type: " ++ name

-- | Parses a single field within a struct or union.
-- Each field consists of a name followed by "->" and its type.
-- Example: "x -> int".
parseField :: AU.Parser (String, AT.Type)
parseField = do
  fieldName <- AU.identifier
  _ <- AU.symbol "->"
  fieldType <- parseType
  return (fieldName, fieldType)
