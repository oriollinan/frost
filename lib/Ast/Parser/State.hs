module Ast.Parser.State where

import qualified Ast.Types as AT
import qualified Data.Set as Set

type TypeState = [(String, AT.Type)]

type VarState = [(String, AT.Type)]

data ImportState = ImportState
  { visitedImports :: Set.Set String,
    recursionDepth :: Int
  }

data ParserState = ParserState
  { typeState :: TypeState,
    varState :: VarState,
    importState :: ImportState
  }

parserState :: ParserState
parserState = ParserState [] [] (ImportState Set.empty 0)

-- | Inserts a custom type into the environment.
-- If the type already exists, it overwrites it.
insertType :: String -> AT.Type -> ParserState -> ParserState
insertType name t s = s {typeState = (name, t) : typeState s}

-- | Looks up a custom type in the environment by its name.
lookupType :: String -> ParserState -> Maybe AT.Type
lookupType name (ParserState {typeState = types}) = lookup name types

insertVar :: String -> AT.Type -> ParserState -> ParserState
insertVar name t s = s {varState = (name, t) : varState s}

lookupVar :: String -> ParserState -> Maybe AT.Type
lookupVar name (ParserState {varState = vars}) = lookup name vars

insertImport :: String -> ParserState -> ParserState
insertImport i s@(ParserState _ _ (ImportState vi r)) = s {importState = ImportState (Set.insert i vi) r}

lookupImport :: String -> ParserState -> Bool
lookupImport i (ParserState _ _ (ImportState vi _))
  | Set.member i vi = True
  | otherwise = False

setImportDepth :: Int -> ParserState -> ParserState
setImportDepth d s@(ParserState _ _ (ImportState vi _)) = s {importState = ImportState vi d}

getImportDepth :: ParserState -> Int
getImportDepth (ParserState _ _ (ImportState _ d)) = d
