module Ast.Env (Env (..), emptyEnv, insertVar, insertFn, lookupVar, lookupFn) where

import Ast.Types (Expr)
import qualified Data.Map as Map

-- | The `Env` data type represents an environment that stores mappings
-- of variable names to their values and function names to their definitions.
data Env = Env
  { fn :: Map.Map String Expr,
    var :: Map.Map String Expr
  }

-- | Creates an empty environment with no variables or functions.
-- `emptyEnv` returns an `Env` instance with empty mappings for both variables and functions.
emptyEnv :: Env
emptyEnv = Env {fn = Map.empty, var = Map.empty}

-- | Inserts a variable into the environment.
-- `insertVar` takes as parameters:
-- - `String`: The name of the variable.
-- - `Expr`: The value of the variable.
-- - `Env`: The current environment.
-- And returns a new `Env` with the variable added to the `var` mapping.
insertVar :: String -> Expr -> Env -> Env
insertVar name value env = env {var = Map.insert name value (var env)}

-- | Inserts a function into the environment.
-- `insertFn` takes as parameters:
-- - `String`: The name of the function.
-- - `Expr`: The definition of the function (usually a lambda expression).
-- - `Env`: The current environment.
-- and returns a new `Env` with the function added to the `fn` mapping.
insertFn :: String -> Expr -> Env -> Env
insertFn name lambda env = env {fn = Map.insert name lambda (fn env)}

-- | Looks up a variable in the environment by its name.
-- `lookupVar` takes as parameters:
-- - `String`: The name of the variable.
-- - `Env`: The current environment.
-- and returns:
-- - `Just Expr`: The value of the variable if it exists in the `var` mapping.
-- - `Nothing`: If the variable does not exist in the environment.
lookupVar :: String -> Env -> Maybe Expr
lookupVar name env = Map.lookup name (var env)

-- | Looks up a function in the environment by its name.
-- `lookupFn` takes as parameters:
-- - `String`: The name of the function.
-- - `Env`: The current environment.
-- and returns:
-- - `Just Expr`: The definition of the function if it exists in the `fn` mapping.
-- - `Nothing`: If the function does not exist in the environment.
lookupFn :: String -> Env -> Maybe Expr
lookupFn name env = Map.lookup name (fn env)
