module Ast.Types
  ( AST (..),
    Literal (..),
    Expr (..),
    Operation (..),
  )
where

-- | Literal values that can appear in the LISP AST.
data Literal
  = LInt Integer
  | LBool Bool
  | LSymbol String
  deriving (Show, Eq)

-- | Expression nodes in the LISP AST.
data Expr
  = Lit Literal
  | Var String
  | Call Expr [Expr]
  | Lambda [String] Expr
  | If Expr Expr Expr
  | Op Operation Expr Expr
  deriving (Show, Eq)

-- | Operations supported by the language, such as addition, subtraction, etc.
data Operation
  = Add
  | Subtract
  | Multiply
  | Divide
  | LessThan
  | Equal
  | And
  | Or
  deriving (Show, Eq)

-- | Top-level AST representation for the program.
data AST
  = Define String Expr
  | Expr Expr
  deriving (Show, Eq)
