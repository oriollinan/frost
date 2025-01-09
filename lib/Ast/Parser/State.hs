module Ast.Parser.State where

import qualified Ast.Types as AT

type TypeState = [(String, AT.Type)]

type VarState = [(String, AT.Type)]

data ParserState = ParserState
  { typeState :: TypeState,
    varState :: VarState
  }

parserState :: ParserState
parserState = ParserState {typeState = [], varState = []}

-- | Inserts a custom type into the environment.
-- If the type already exists, it overwrites it.
insertType :: String -> AT.Type -> ParserState -> ParserState
insertType name t s = s {typeState = (name, t) : typeState s}

-- | Looks up a custom type in the environment by its name.
lookupType :: String -> ParserState -> Maybe AT.Type
lookupType name (ParserState types _) = lookup name types

insertVar :: String -> AT.Type -> ParserState -> ParserState
insertVar name t s = s {varState = (name, t) : varState s}

lookupVar :: String -> ParserState -> Maybe AT.Type
lookupVar name (ParserState _ vars) = lookup name vars
