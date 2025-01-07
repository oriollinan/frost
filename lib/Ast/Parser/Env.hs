module Ast.Parser.Env where

import qualified Ast.Types as AT

-- | The environment for storing custom type definitions.
data Env = Env
  { types :: [(String, AT.Type)],
    vars :: [(String, AT.Type)]
  }

-- | Creates an empty environment.
emptyEnv :: Env
emptyEnv = Env {types = [], vars = []}

-- | Inserts a custom type into the environment.
-- If the type already exists, it overwrites it.
insertType :: String -> AT.Type -> Env -> Env
insertType name t env = env {types = (name, t) : types env}

-- | Looks up a custom type in the environment by its name.
lookupType :: String -> Env -> Maybe AT.Type
lookupType name env = lookup name $ types env

insertVar :: String -> AT.Type -> Env -> Env
insertVar name t env = env {vars = (name, t) : vars env}

lookupVar :: String -> Env -> Maybe AT.Type
lookupVar name env = lookup name $ vars env
