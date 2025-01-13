module Ast.Parser.State where

import qualified Ast.Types as AT
import Debug.Trace

type TypeState = [(String, AT.Type)]

type VarState = [(String, AT.Type)]

type DeferState = [[AT.Expr]]

data ParserState = ParserState
  { typeState :: TypeState,
    varState :: VarState,
    deferState :: DeferState
  }
  deriving (Show, Eq)

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

-- | Pushes a deferred expression onto the current defer stack.
pushDefered :: AT.Expr -> ParserState -> ParserState
pushDefered e s@(ParserState {deferState = ds}) = case ds of
  [] -> s {deferState = [[e]]}
  (current : rest) -> s {deferState = (e : current) : rest}

-- | Pushes a new empty array onto the defer state to represent entering a new scope.
pushScope :: ParserState -> ParserState
pushScope s@(ParserState {deferState = ds}) =
  let newState = s {deferState = [] : ds}
   in trace ("pushScope: " ++ show newState) newState

-- | Pops the top scope expressions stack.
-- Returns the popped stack and the updated ParserState.
popScope :: ParserState -> ([AT.Expr], ParserState)
popScope s@(ParserState {deferState = ds}) =
  case ds of
    [] -> ([], s) -- No deferred expressions to pop.
    (top : rest) -> (top, s {deferState = rest}) -- Pop the top stack and update state.
