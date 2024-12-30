module Ast.Parser.Type where

import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import Data.Functor (($>))
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

-- | Parse a general type. This function combines multiple specific type parsers.
-- It tries to match typedefs, structs, unions, functions, mutable types, pointers, and base types.
parseType :: AU.Parser AT.Type
parseType = AU.triedChoice [structType, unionType, typedefType, functionType, mutableType, pointerType, baseType]

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
-- A mutable type is prefixed by the keyword "mutable" followed by the type.
-- Example: "mutable int" indicates a mutable integer type.
mutableType :: AU.Parser AT.Type
mutableType = AT.TMutable <$> (MC.string "mutable" *> AU.sc *> parseType)

-- | Parses a struct type definition.
-- A struct is defined with the "struct" keyword followed by an optional name and a list of fields enclosed in braces.
-- Example: "struct { x -> int, y -> float }".
structType :: AU.Parser AT.Type
structType = do
  name <- identifier
  _ <- AU.symbol "::" <* AU.symbol "struct"
  fields <- M.between (MC.char '{') (MC.char '}') $ M.many (AU.sc *> parseField)
  return $ AT.TStruct {AT.structName = name, AT.fields = fields}

-- | Parses a union type definition.
-- A union is defined with the "union" keyword followed by an optional name and a list of variants enclosed in braces.
-- Example: "union { data -> *char, error -> int }".
unionType :: AU.Parser AT.Type
unionType = do
  name <- identifier
  _ <- AU.symbol "::" <* AU.symbol "union"
  variants <- M.between (MC.char '{') (MC.char '}') $ M.many (AU.sc *> parseField)
  return $ AT.TUnion {AT.unionName = name, AT.variants = variants}

-- | Parses a typedef.
-- A typedef associates a new name with an existing type using the "::" syntax.
-- Example: "Vector2i :: struct { x -> int, y -> int }".
typedefType :: AU.Parser AT.Type
typedefType = do
  name <- identifier
  _ <- AU.symbol "::"
  AT.TTypedef name <$> parseType

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

identifier :: AU.Parser String
identifier = AU.lexeme ((:) <$> MC.letterChar <*> M.many MC.alphaNumChar)

-- | Parses a single field within a struct or union.
-- Each field consists of a name followed by "->" and its type.
-- Example: "x -> int".
parseField :: AU.Parser (String, AT.Type)
parseField = do
  fieldName <- identifier
  _ <- AU.symbol "->"
  fieldType <- parseType
  return (fieldName, fieldType)
