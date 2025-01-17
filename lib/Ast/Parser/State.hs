module Ast.Parser.State where

import qualified Ast.Types as AT
import qualified Data.Set as Set

type TypeState = [(String, AT.Type)]

type VarState = [(String, AT.Type)]

data PreProcessorState = PreProcessorState
  { visitedImports :: Set.Set String,
    importDepth :: Int
  }
  deriving (Show, Eq)

type DeferState = [[AT.Expr]]

data ParserState = ParserState
  { typeState :: TypeState,
    varState :: VarState,
    preProcessorState :: PreProcessorState,
    deferState :: DeferState
  }
  deriving (Show, Eq)

parserState :: ParserState
parserState = ParserState [] [] (PreProcessorState Set.empty 0) []

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
insertImport i s =
  s {preProcessorState = (preProcessorState s) {visitedImports = Set.insert i $ visitedImports . preProcessorState $ s}}

lookupImport :: String -> ParserState -> Bool
lookupImport i (ParserState {preProcessorState = PreProcessorState {visitedImports = vi}})
  | Set.member i vi = True
  | otherwise = False

setImportDepth :: Int -> ParserState -> ParserState
setImportDepth d s = s {preProcessorState = (preProcessorState s) {importDepth = d}}

getImportDepth :: ParserState -> Int
getImportDepth (ParserState {preProcessorState = PreProcessorState {importDepth = d}}) = d

-- | Pushes a deferred expression onto the current defer stack.
pushDefered :: AT.Expr -> ParserState -> ParserState
pushDefered e s@(ParserState {deferState = ds}) = case ds of
  [] -> s {deferState = [[e]]}
  (current : rest) -> s {deferState = (e : current) : rest}

-- | Pushes a new empty array onto the defer state to represent entering a new scope.
pushScope :: ParserState -> ParserState
pushScope s@(ParserState {deferState = ds}) = s {deferState = [] : ds}

-- | Pops the top scope expressions stack.
-- Returns the popped stack and the updated ParserState.
popScope :: ParserState -> ([AT.Expr], ParserState)
popScope s@(ParserState {deferState = ds}) =
  case ds of
    [] -> ([], s) -- No deferred expressions to pop.
    (top : rest) -> (top, s {deferState = rest}) -- Pop the top stack and update state.
