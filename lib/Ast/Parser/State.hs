module Ast.Parser.State where

import qualified Ast.Types as AT
import qualified Data.Set as Set

-- | Represents the state of type definitions in the parser.
type TypeState = [(String, AT.Type)]

-- | Represents the state of variable definitions in the parser.
type VarState = [(String, AT.Type)]

-- | Tracks preprocessor-specific information, such as visited imports and import depth.
data PreProcessorState = PreProcessorState
  { visitedImports :: Set.Set String,
    importDepth :: Int
  }
  deriving (Show, Eq)

-- | Represents the stack of deferred expressions, grouped by scope.
type DeferState = [[AT.Expr]]

-- | The complete state of the parser, including types, variables, preprocessor state, and deferred expressions.
data ParserState = ParserState
  { typeState :: TypeState,
    varState :: VarState,
    preProcessorState :: PreProcessorState,
    deferState :: DeferState
  }
  deriving (Show, Eq)

-- | The initial state of the parser.
parserState :: ParserState
parserState = ParserState [] [] (PreProcessorState Set.empty 0) []

-- | Inserts a custom type into the environment.
-- If the type already exists, it overwrites it.
insertType :: String -> AT.Type -> ParserState -> ParserState
insertType name t s = s {typeState = (name, t) : typeState s}

-- | Looks up a custom type in the environment by its name.
lookupType :: String -> ParserState -> Maybe AT.Type
lookupType name (ParserState {typeState = types}) = lookup name types

-- | Inserts a variable into the environment.
-- If the variable already exists, it overwrites it.
insertVar :: String -> AT.Type -> ParserState -> ParserState
insertVar name t s = s {varState = (name, t) : varState s}

-- | Looks up a variable in the environment by its name.
-- Returns `Nothing` if the variable is not found.
lookupVar :: String -> ParserState -> Maybe AT.Type
lookupVar name (ParserState {varState = vars}) = lookup name vars

-- | Marks an import as visited by adding it to the set of visited imports.
insertImport :: String -> ParserState -> ParserState
insertImport i s =
  s {preProcessorState = (preProcessorState s) {visitedImports = Set.insert i $ visitedImports . preProcessorState $ s}}

-- | Checks if a file has already been imported.
-- Returns `True` if the file is in the set of visited imports.
lookupImport :: String -> ParserState -> Bool
lookupImport i (ParserState {preProcessorState = PreProcessorState {visitedImports = vi}})
  | Set.member i vi = True
  | otherwise = False

-- | Sets the depth of nested imports in the preprocessor state.
setImportDepth :: Int -> ParserState -> ParserState
setImportDepth d s = s {preProcessorState = (preProcessorState s) {importDepth = d}}

-- | Gets the current depth of nested imports from the preprocessor state.
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
    [] -> ([], s)
    (top : rest) -> (top, s {deferState = rest})
