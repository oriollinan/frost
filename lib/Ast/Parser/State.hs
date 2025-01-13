module Ast.Parser.State where

import qualified Ast.Types as AT

type TypeState = [(String, AT.Type)]

type VarState = [(String, AT.Type)]

type DeferState = [[AT.Expr]]

data ParserState = ParserState
  { typeState :: TypeState,
    varState :: VarState,
    deferState :: DeferState
  }

parserState :: ParserState
parserState = ParserState {typeState = [], varState = [], deferState = []}

-- | Inserts a custom type into the environment.
-- If the type already exists, it overwrites it.
insertType :: String -> AT.Type -> ParserState -> ParserState
insertType name t s = s {typeState = (name, t) : typeState s}

-- | Looks up a custom type in the environment by its name.
lookupType :: String -> ParserState -> Maybe AT.Type
lookupType name (ParserState types _ _) = lookup name types

insertVar :: String -> AT.Type -> ParserState -> ParserState
insertVar name t s = s {varState = (name, t) : varState s}

lookupVar :: String -> ParserState -> Maybe AT.Type
lookupVar name (ParserState _ vars _) = lookup name vars

pushDefered :: AT.Expr -> ParserState -> ParserState
pushDefered e s@(ParserState {deferState = ds}) = case ds of
  [] -> s {deferState = [[e]]}
  [first : rest] -> s {deferState = [e : first, rest]}

insertDefered :: AT.Expr -> ParserState -> ParserState
insertDefered e s@(ParserState {deferState = ds}) = s {deferState = e : ds}
