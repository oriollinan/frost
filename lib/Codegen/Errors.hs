module Codegen.Errors where

import qualified Ast.Types as AT
import qualified LLVM.AST.Type as T

-- | Error types for code generation.
data CodegenError = CodegenError
  { errorLoc :: AT.SrcLoc,
    errorType :: CodegenErrorType
  }

-- | Error types for code generation.
data CodegenErrorType
  = UnsupportedTopLevel AT.Expr
  | UnsupportedOperator AT.Operation
  | UnsupportedUnaryOperator AT.UnaryOperation
  | UnsupportedLiteral AT.Literal
  | UnsupportedType AT.Type
  | UnsupportedGlobalVar AT.Literal
  | UnsupportedLocalVar AT.Literal
  | UnsupportedDefinition AT.Expr
  | UnsupportedForDefinition AT.Expr
  | UnsupportedWhileDefinition AT.Expr
  | VariableNotFound String
  | UnsupportedFunctionCall String
  | UnsupportedStructureAccess AT.Expr
  | StructureFieldNotFound String
  | ContinueOutsideLoop
  | BreakOutsideLoop
  | UnsupportedConversion T.Type T.Type
  | UnsupportedGlobalDeclaration AT.Expr
  deriving (Show)

instance Show CodegenError where
  show (CodegenError loc err) =
    AT.srcFile loc
      ++ ":"
      ++ show (AT.srcLine loc)
      ++ ":"
      ++ show (AT.srcCol loc)
      ++ ": "
      ++ showErrorType err

-- | Show error type as string.
showErrorType :: CodegenErrorType -> String
showErrorType err = case err of
  UnsupportedTopLevel expr -> "Unsupported top-level expression: " ++ show expr
  UnsupportedOperator op -> "Unsupported operator: " ++ show op
  UnsupportedUnaryOperator op -> "Unsupported unary operator: " ++ show op
  UnsupportedLiteral lit -> "Unsupported literal: " ++ show lit
  UnsupportedType typ -> "Unsupported type: " ++ show typ
  UnsupportedGlobalVar lit -> "Unsupported global variable: " ++ show lit
  UnsupportedLocalVar lit -> "Unsupported local variable: " ++ show lit
  UnsupportedDefinition expr -> "Unsupported definition: " ++ show expr
  UnsupportedForDefinition expr -> "Invalid for loop: " ++ show expr
  UnsupportedWhileDefinition expr -> "Invalid while loop: " ++ show expr
  VariableNotFound name -> "Variable not found: " ++ name
  UnsupportedFunctionCall name -> "Invalid function call: " ++ name
  UnsupportedStructureAccess expr -> "Invalid structure access: " ++ show expr
  StructureFieldNotFound name -> "Structure field not found: " ++ name
  ContinueOutsideLoop -> "Continue statement outside loop"
  BreakOutsideLoop -> "Break statement outside loop"
  UnsupportedConversion from to -> "Unsupported conversion from " ++ show from ++ " to " ++ show to
  UnsupportedGlobalDeclaration expr -> "Unsupported global declaration: " ++ show expr
