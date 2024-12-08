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
  | Define String Expr
  | Call Expr Expr
  | Lambda [String] Expr
  | If Expr Expr Expr
  | Op Operation Expr Expr
  | Seq [Expr]
  deriving (Show, Eq, Ord)

-- | Operations supported by the language, such as addition, subtraction, etc.
data Operation
  = Add
  | Sub
  | Mult
  | Div
  | Mod
  | Lt
  | Gt
  | Lte
  | Gte
  | Equal
  | Ne
  | And
  | Or
  deriving (Show, Eq, Ord)

-- | Top-level AST representation for the program.
newtype AST = AST [Expr] deriving (Show, Eq)
