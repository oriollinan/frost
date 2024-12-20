module Ast.Parser.Type where

import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import Data.Functor (($>))
import Text.Megaparsec ((<|>))
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as ML

parseType :: AU.Parser AT.Type
parseType = AU.triedChoice [structType, mutableType, typedefType, pointerType, arrayType, baseType]

baseTypes :: [(String, AT.Type)]
baseTypes =
  [ ("int", AT.TInt 0),
    ("float", AT.TFloat),
    ("double", AT.TDouble),
    ("char", AT.TChar),
    ("bool", AT.TBoolean),
    ("void", AT.TVoid)
  ]

-- | Parses a base type using the `baseTypes` list.
baseType :: AU.Parser AT.Type
baseType = M.choice $ (\(kw, ty) -> ty <$ AU.symbol kw) <$> baseTypes

-- | Parse a pointer type (e.g., `*int`)
pointerType :: AU.Parser AT.Type
pointerType = AT.TPointer <$> (MC.char '*' *> parseType)

mutableType :: AU.Parser AT.Type
mutableType = AT.TMutable <$> (MC.string "mutable" *> AU.sc *> parseType)

-- ! fix not working properly, infinite loop because of `parseType`
arrayType :: AU.Parser AT.Type
arrayType = do
  t <- parseType
  size <- M.optional $ M.between (AU.symbol "[") (AU.symbol "]") ML.decimal
  pure (AT.TArray t size)

-- ! fix not working properly, infinite loop because of `parseType`
functionType :: AU.Parser AT.Type
functionType = do
  paramTypes <- M.some parseType
  _ <- AU.symbol "->"
  returnType <- parseType
  pure (AT.TFunction returnType paramTypes False)

-- TODO: after having a decided syntax for them implement `struct`, `union` and `typedef`
structType :: AU.Parser AT.Type
structType = pure (AT.TStruct "placeholder" [("field", AT.TInt 0)])

unionType :: AU.Parser AT.Type
unionType = pure (AT.TUnion "placeholder" [("variant", AT.TFloat)])

typedefType :: AU.Parser AT.Type
typedefType = pure (AT.TTypedef "placeholder" AT.TChar)

identifier :: AU.Parser String
identifier = AU.lexeme ((:) <$> MC.letterChar <*> M.many MC.letterChar)
