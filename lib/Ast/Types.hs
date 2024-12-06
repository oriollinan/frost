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
  deriving (Show, Eq, Ord)

-- | Expression nodes in the LISP AST.
data Expr
  = Lit Literal
  | Var String
  | Define AST AST
  | Call AST [AST]
  | Lambda [String] AST
  | If AST AST AST
  | Op Operation AST AST
  deriving (Show, Eq)

-- | Operations supported by the language, such as addition, subtraction, etc.
data Operation
  = Add
  | Sub
  | Mult
  | Div
  | Lt
  | Gt
  | Lte
  | Gte
  | Equal
  | And
  | Or
  deriving (Show, Eq)

-- | Top-level AST representation for the program.
data AST = Atom Expr | List [AST] deriving (Show, Eq)
