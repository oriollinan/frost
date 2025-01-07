module Ast.Parser.Operation where

import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Text.Megaparsec as M

-- | Operator symbols mapped to their AST representation.
operations :: [(String, AT.Operation)]
operations =
  [ ("+", AT.Add),
    ("-", AT.Sub),
    ("*", AT.Mul),
    ("/", AT.Div),
    ("mod", AT.Mod),
    ("&&", AT.And),
    ("and", AT.And),
    ("||", AT.Or),
    ("or", AT.Or),
    ("&", AT.BitAnd),
    ("|", AT.BitOr),
    ("^", AT.BitXor),
    ("<<", AT.BitShl),
    (">>", AT.BitShr),
    ("<=", AT.Lte),
    (">=", AT.Gte),
    ("<", AT.Lt),
    (">", AT.Gt),
    ("==", AT.Eq),
    ("is", AT.Eq),
    ("!=", AT.Ne)
  ]

-- | Parses a symbol into an `AT.Operation` using the `operations` list.
-- Returns the corresponding `AT.Operation` if a match is found.
parseOperation :: PU.Parser AT.Operation
parseOperation = M.choice $ (\(o, c) -> c <$ PU.symbol o) <$> operations
