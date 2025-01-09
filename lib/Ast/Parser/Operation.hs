module Ast.Parser.Operation where

import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.Combinators.Expr as CE

-- | Operations that are defined, including verbs
operationTable :: [[CE.Operator PU.Parser AT.Expr]]
operationTable =
  [ [ prefix "!" (`AT.UnaryOp` AT.Not),
      prefix "not" (`AT.UnaryOp` AT.Not),
      prefix "~" (`AT.UnaryOp` AT.BitNot),
      prefix "&" (`AT.UnaryOp` AT.AddrOf),
      prefix "++" (`AT.UnaryOp` AT.PreInc),
      prefix "--" (`AT.UnaryOp` AT.PreDec)
    ],
    [ binary "*" (`AT.Op` AT.Mul),
      binary "/" (`AT.Op` AT.Div),
      binary "mod" (`AT.Op` AT.Mod)
    ],
    [ binary "+" (`AT.Op` AT.Add),
      binary "-" (`AT.Op` AT.Sub)
    ],
    [ binary "&" (`AT.Op` AT.BitAnd),
      binary "|" (`AT.Op` AT.BitOr),
      binary "^" (`AT.Op` AT.BitXor),
      binary "<<" (`AT.Op` AT.BitShl),
      binary ">>" (`AT.Op` AT.BitShr)
    ],
    [ binary "&&" (`AT.Op` AT.And),
      binary "and" (`AT.Op` AT.And),
      binary "||" (`AT.Op` AT.Or),
      binary "or" (`AT.Op` AT.Or),
      binary "==" (`AT.Op` AT.Eq),
      binary "is" (`AT.Op` AT.Eq),
      binary "!=" (`AT.Op` AT.Ne),
      binary "<=" (`AT.Op` AT.Lte),
      binary ">=" (`AT.Op` AT.Gte),
      binary "<" (`AT.Op` AT.Lt),
      binary ">" (`AT.Op` AT.Gt)
    ],
    [ postfix "." (`AT.UnaryOp` AT.Deref),
      postfix "++" (`AT.UnaryOp` AT.PostInc),
      postfix "--" (`AT.UnaryOp` AT.PostDec)
    ]
  ]

prefix :: String -> (AT.SrcLoc -> AT.Expr -> AT.Expr) -> CE.Operator PU.Parser AT.Expr
prefix name f = CE.Prefix (f <$> (PU.parseSrcLoc <* PU.symbol name))

postfix :: String -> (AT.SrcLoc -> AT.Expr -> AT.Expr) -> CE.Operator PU.Parser AT.Expr
postfix name f = CE.Postfix (f <$> (PU.parseSrcLoc <* PU.symbol name))

-- | Helper functions to define operators
binary :: String -> (AT.SrcLoc -> AT.Expr -> AT.Expr -> AT.Expr) -> CE.Operator PU.Parser AT.Expr
binary name f = CE.InfixL (f <$> (PU.parseSrcLoc <* PU.symbol name))
