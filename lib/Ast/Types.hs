module Ast.Types where

-- | Source location for better error reporting
data SrcLoc = SrcLoc
  { srcFile :: String,
    srcLine :: Int,
    srcCol :: Int
  }
  deriving (Show, Eq, Ord)

-- | Enhanced literal values including characters and floating-point numbers
data Literal
  = LInt Integer
  | LFloat Double
  | LChar Char
  | LBool Bool
  | LArray [Literal]
  | LNull
  | LStruct [(String, Literal)]
  deriving (Show, Eq, Ord)

-- | Enhanced type system with size information and qualifiers
-- | TInt: Int with bit width (8, 16, 32, 64)
-- | TFloat: 32-bit float
-- | TDouble: 64-bit float
-- | TArray: Array type with optional size
-- | TTypedef: Type aliases
data Type
  = TInt Int
  | TFloat
  | TDouble
  | TChar
  | TBoolean
  | TVoid
  | TMutable Type
  | TPointer Type
  | TArray Type (Maybe Int)
  | TFunction
      { returnType :: Type,
        paramTypes :: [Type],
        isVariadic :: Bool
      }
  | TStruct
      { structName :: String,
        fields :: [(String, Type)]
      }
  | TUnion
      { unionName :: String,
        variants :: [(String, Type)]
      }
  | TTypedef String Type
  | TUnknown
  deriving (Show, Eq, Ord)

-- | Assembly constraint type
data AsmConstraint = AsmConstraint
  { outputConstraint :: String,
    inputConstraints :: [String]
  }
  deriving (Show, Eq, Ord)

-- | Assembly expression type
data AsmExpr = AsmExpr
  { asmCode :: String,
    asmConstraints :: AsmConstraint,
    asmArgs :: [Expr],
    asmSideEffects :: Bool,
    asmAlignStack :: Bool
  }
  deriving (Show, Eq, Ord)

-- | Enhanced expression nodes
-- | StructAccess: For accessing struct fields
-- | ArrayAccess: For array indexing
data Expr
  = Lit SrcLoc Literal
  | Var SrcLoc String Type
  | Function
      { funcLoc :: SrcLoc,
        funcName :: String,
        funcType :: Type,
        funcParams :: [String],
        funcBody :: Expr
      }
  | ForeignFunction
      { funcLoc :: SrcLoc,
        funcName :: String,
        funcType :: Type
      }
  | Declaration
      { declLoc :: SrcLoc,
        declName :: String,
        declType :: Type,
        declInit :: Maybe Expr
      }
  | Assignment
      { assignLoc :: SrcLoc,
        assignTarget :: Expr,
        assignValue :: Expr
      }
  | Call
      { callLoc :: SrcLoc,
        callFunc :: Expr,
        callArgs :: [Expr]
      }
  | If
      { ifLoc :: SrcLoc,
        ifCond :: Expr,
        ifThen :: Expr,
        ifElse :: Maybe Expr
      }
  | While
      { whileLoc :: SrcLoc,
        whileCond :: Expr,
        whileBody :: Expr
      }
  | For
      { forLoc :: SrcLoc,
        forInit :: Expr,
        forCond :: Expr,
        forStep :: Expr,
        forBody :: Expr
      }
  | Block [Expr]
  | Return SrcLoc (Maybe Expr)
  | Break SrcLoc
  | Continue SrcLoc
  | Op SrcLoc Operation Expr Expr
  | UnaryOp SrcLoc UnaryOperation Expr
  | StructAccess SrcLoc Expr Expr
  | ArrayAccess SrcLoc Expr Expr
  | Cast SrcLoc Type Expr
  | Assembly
      { asmLoc :: SrcLoc,
        asmReturnType :: Type,
        asmExpr :: AsmExpr
      }
  deriving (Show, Eq, Ord)

-- | Enhanced operations including bitwise operations
data Operation
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | BitShl
  | BitShr
  | Lt
  | Gt
  | Lte
  | Gte
  | Eq
  | Ne
  | And
  | Or
  deriving (Show, Eq, Ord)

-- | Unary operations
-- | Not: Logical not
-- | BitNot: Bitwise not
-- | Deref: Pointer dereference
-- | AddrOf: Address-of operator
-- | PreInc: Pre-increment
-- | PreDec: Pre-decrement
-- | PostInc: Post-increment
-- | PostDec: Post-decrement
data UnaryOperation
  = Not
  | BitNot
  | Deref
  | AddrOf
  | PreInc
  | PreDec
  | PostInc
  | PostDec
  deriving (Show, Eq, Ord)

-- | Program representation with global scope information
-- | globals: Global variables and functions
-- | types: Type definitions
-- | sourceFile: Source file name
data Program = Program
  { globals :: [(String, Expr)],
    types :: [(String, Type)],
    sourceFile :: String
  }
  deriving (Show, Eq)
