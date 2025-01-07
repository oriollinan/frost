module Ast.Parser.UnaryOperation where

import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Text.Megaparsec as M

-- | Defines unary operator symbols that precede the operand, mapped to their AST representation.
-- Examples: `++x`, `--x`, `!x`.
preUnaryOperations :: [(String, AT.UnaryOperation)]
preUnaryOperations =
  [ ("!", AT.Not),
    ("not", AT.Not),
    ("~", AT.BitNot),
    ("&", AT.AddrOf),
    ("++", AT.PreInc),
    ("--", AT.PreDec)
  ]

-- | Defines unary operator symbols that follow the operand, mapped to their AST representation.
-- Examples: `x++`, `x--`.
postUnaryOperations :: [(String, AT.UnaryOperation)]
postUnaryOperations =
  [ (".", AT.Deref),
    ("++", AT.PostInc),
    ("--", AT.PostDec)
  ]

-- | Parses a unary operator and determines whether it appears before or after the operand.
--
-- The function uses lookahead to avoid consuming the operand during parsing.
-- - Pre-unary operators are parsed if they appear before the operand.
-- - Post-unary operators are parsed if they appear after the operand.
--
-- `operandParser`: A parser for the operand, used to determine the position of the operator.
parseUnaryOperation :: PU.Parser a -> PU.Parser AT.UnaryOperation
parseUnaryOperation operandParser =
  M.choice
    [ parseUnaryOperation' preUnaryOperations <* M.lookAhead operandParser,
      M.lookAhead (operandParser *> parseUnaryOperation' postUnaryOperations)
    ]
  where
    parseUnaryOperation' :: [(String, AT.UnaryOperation)] -> PU.Parser AT.UnaryOperation
    parseUnaryOperation' ops = M.choice $ (\(o, c) -> c <$ PU.symbol o) <$> ops
