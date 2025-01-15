{-# LANGUAGE FlexibleContexts #-}

module Codegen.ExprGen.Types where

import qualified Ast.Types as AT
import qualified LLVM.AST.Type as T

-- | Type conversion to LLVM IR.
class ToLLVM a where
  toLLVM :: a -> T.Type

instance ToLLVM AT.Type where
  toLLVM expr = case expr of
    AT.TInt width -> T.IntegerType (fromIntegral width)
    AT.TFloat -> T.FloatingPointType T.FloatFP
    AT.TDouble -> T.FloatingPointType T.DoubleFP
    AT.TChar -> T.IntegerType 8
    AT.TBoolean -> T.IntegerType 1
    AT.TVoid -> T.void
    AT.TPointer t -> T.ptr (toLLVM t)
    AT.TArray t (Just n) -> T.ArrayType (fromIntegral n) (toLLVM t)
    AT.TArray t Nothing -> T.ptr (toLLVM t)
    AT.TFunction ret params var -> T.FunctionType (toLLVM ret) (map toLLVM params) var
    AT.TStruct _ fields -> T.StructureType False (map (toLLVM . snd) fields)
    AT.TUnion _ variants -> T.StructureType False (map (toLLVM . snd) variants)
    AT.TTypedef _ t -> toLLVM t
    AT.TMutable t -> toLLVM t
    AT.TUnknown -> T.void
