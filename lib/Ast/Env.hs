module Ast.Env (Env (..), emptyEnv, insertVar, insertFn, lookupVar, lookupFn) where

import Ast.Types (Expr)
import qualified Data.Map as Map

data Env = Env
  { fn :: Map.Map String Expr,
    var :: Map.Map String Expr
  }

emptyEnv :: Env
emptyEnv = Env {fn = Map.empty, var = Map.empty}

insertVar :: String -> Expr -> Env -> Env
insertVar name value env = env {var = Map.insert name value (var env)}

insertFn :: String -> Expr -> Env -> Env
insertFn name lambda env = env {fn = Map.insert name lambda (fn env)}

lookupVar :: String -> Env -> Maybe Expr
lookupVar name env = Map.lookup name (var env)

lookupFn :: String -> Env -> Maybe Expr
lookupFn name env = Map.lookup name (fn env)
