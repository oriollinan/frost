module Ast.Parser.TypeDefinition where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Type as T
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

-- | Parse a type definition. This function combines multiple specific type
-- definition.
-- It tries to match typedefs, structs, and unions.
parseTypeDefinition :: PU.Parser AT.Type
parseTypeDefinition =
  M.choice
    [ M.try structType,
      M.try unionType,
      typedefType
    ]

-- | Parses a struct type definition.
-- A struct is defined with the "struct" keyword followed by an optional name and a list of fields enclosed in braces.
-- Example: "struct { x -> int, y -> float }".
structType :: PU.Parser AT.Type
structType = do
  name <- PU.identifier
  _ <- PU.symbol "::" <* PU.symbol "struct"
  fields <- M.between (PU.symbol "{") (PU.symbol "}") $ M.many parseField
  let newStructType = AT.TStruct {AT.structName = name, AT.fields = fields}
  S.modify (PS.insertType name newStructType)
  return newStructType

-- | Parses a union type definition.
-- A union is defined with the "union" keyword followed by an optional name and a list of variants enclosed in braces.
-- Example: "union { data -> *char, error -> int }".
unionType :: PU.Parser AT.Type
unionType = do
  name <- PU.identifier
  _ <- PU.symbol "::" <* PU.symbol "union"
  variants <- M.between (PU.symbol "{") (PU.symbol "}") $ M.many parseField
  let newUnionType = AT.TUnion {AT.unionName = name, AT.variants = variants}
  S.modify (PS.insertType name newUnionType)
  return newUnionType

-- | Parses a typedef.
-- A typedef associates a new name with an existing type using the "::" syntax.
-- Example: "Vector2i :: Vector".
typedefType :: PU.Parser AT.Type
typedefType = do
  name <- PU.identifier
  _ <- PU.symbol "::"
  parentType <- T.parseType
  let typedef = AT.TTypedef name parentType
  S.modify (PS.insertType name typedef)
  return typedef

-- | Parses a single field within a struct or union.
-- Each field consists of a name followed by "->" and its type.
-- Example: "x -> int".
parseField :: PU.Parser (String, AT.Type)
parseField = do
  fieldName <- PU.lexeme PU.identifier
  _ <- PU.symbol "->"
  fieldType <- T.parseType
  return (fieldName, fieldType)
