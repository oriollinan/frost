module Ast.Parser.TypeDefinition where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Type as T
import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

-- | Parse a type definition. This function combines multiple specific type
-- definition.
-- It tries to match typedefs, structs, and unions.
parseTypeDefinition :: AU.Parser ()
parseTypeDefinition = AU.triedChoice [structType, unionType, typedefType]

-- | Parses a struct type definition.
-- A struct is defined with the "struct" keyword followed by an optional name and a list of fields enclosed in braces.
-- Example: "struct { x -> int, y -> float }".
structType :: AU.Parser ()
structType = do
  name <- AU.identifier
  _ <- AU.symbol "::" <* AU.symbol "struct"
  fields <- M.between (AU.symbol "{") (AU.symbol "}") $ M.many parseField
  let newStructType = AT.TStruct {AT.structName = name, AT.fields = fields}
  S.modify (E.insertType name newStructType)
  return ()

-- | Parses a union type definition.
-- A union is defined with the "union" keyword followed by an optional name and a list of variants enclosed in braces.
-- Example: "union { data -> *char, error -> int }".
unionType :: AU.Parser ()
unionType = do
  name <- AU.identifier
  _ <- AU.symbol "::" <* AU.symbol "union"
  variants <- M.between (AU.symbol "{") (AU.symbol "}") $ M.many parseField
  let newUnionType = AT.TUnion {AT.unionName = name, AT.variants = variants}
  S.modify (E.insertType name newUnionType)
  return ()

-- | Parses a typedef.
-- A typedef associates a new name with an existing type using the "::" syntax.
-- Example: "Vector2i :: Vector".
typedefType :: AU.Parser ()
typedefType = do
  name <- AU.identifier
  _ <- AU.symbol "::"
  parentType <- T.parseType
  let typedef = AT.TTypedef name parentType
  S.modify (E.insertType name typedef)
  return ()

-- | Parses a single field within a struct or union.
-- Each field consists of a name followed by "->" and its type.
-- Example: "x -> int".
parseField :: AU.Parser (String, AT.Type)
parseField = do
  fieldName <- AU.lexeme AU.identifier
  _ <- AU.symbol "->"
  fieldType <- T.parseType
  return (fieldName, fieldType)
