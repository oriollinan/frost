{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Codegen.State where

import qualified Codegen.Errors as CC
import qualified Codegen.Utils as CU
import qualified Control.Monad.Except as E
import qualified Control.Monad.Fix as F
import qualified Control.Monad.State as S
import qualified LLVM.AST as AST
import qualified LLVM.IRBuilder.Module as M
import qualified LLVM.IRBuilder.Monad as IRM

type MonadCodegen m =
  ( IRM.MonadIRBuilder m,
    M.MonadModuleBuilder m,
    F.MonadFix m,
    S.MonadState CodegenState m,
    E.MonadError CC.CodegenError m
  )

-- | Type alias for the local code generation state.
type LocalState = [(String, AST.Operand)]

-- | Type alias for the global code generation state.
type GlobalState = [(String, AST.Operand)]

-- | Type alias for the loop code generation state.
type LoopState = Maybe (AST.Name, AST.Name)

-- | Type alias for the variables name .
type UniqueNameState = Integer

-- | Combined state for code generation.
data CodegenState = CodegenState
  { localState :: LocalState,
    globalState :: GlobalState,
    loopState :: LoopState,
    allocatedVars :: LocalState,
    uniqueNameState :: UniqueNameState
  }
  deriving (Show)

-- | Variable binding typeclass.
class (Monad m) => VarBinding m where
  getVar :: String -> m (Maybe AST.Operand)
  addVar :: String -> AST.Operand -> m ()
  getGlobalVar :: String -> m (Maybe AST.Operand)
  addGlobalVar :: String -> AST.Operand -> m ()

instance (S.MonadState CodegenState m, Monad m) => VarBinding m where
  getVar :: (S.MonadState CodegenState m, Monad m) => String -> m (Maybe AST.Operand)
  getVar name = do
    state <- S.get
    return $
      lookup name (allocatedVars state)
        `S.mplus` lookup name (localState state)
        `S.mplus` lookup name (globalState state)

  addVar :: (S.MonadState CodegenState m, Monad m) => String -> AST.Operand -> m ()
  addVar name operand = S.modify (\s -> s {localState = (name, operand) : localState s})

  getGlobalVar :: (S.MonadState CodegenState m, Monad m) => String -> m (Maybe AST.Operand)
  getGlobalVar name = S.gets (lookup name . globalState)

  addGlobalVar :: (S.MonadState CodegenState m, Monad m) => String -> AST.Operand -> m ()
  addGlobalVar name operand = S.modify (\s -> s {globalState = (name, operand) : globalState s})

-- Generates a fresh unique name.
fresh :: (S.MonadState CodegenState m) => m AST.Name
fresh = do
  state <- S.get
  let uniqueName = uniqueNameState state
  S.put $ state {uniqueNameState = uniqueName + 1}
  let fullName = "_" ++ show uniqueName
  return $ AST.Name (CU.stringToByteString fullName)

-- Generates a fresh unique name with the given prefix.
freshName :: (S.MonadState CodegenState m) => String -> m AST.Name
freshName prefix = do
  state <- S.get
  let uniqueName = uniqueNameState state
  S.put $ state {uniqueNameState = uniqueName + 1}
  let fullName = prefix ++ show uniqueName
  return $ AST.Name (CU.stringToByteString fullName)
