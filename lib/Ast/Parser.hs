module Ast.Parser where

import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import qualified Text.Megaparsec as M

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a filename `String`, and an input `String` as a parameter and returns either an AST or an error message.
parse :: String -> String -> Either String AT.Program
parse filename input = case M.runParser parseProgram filename input of
  (Left err) -> Left (M.errorBundlePretty err)
  (Right program) -> Right program

-- | Parses the top-level structure of a program.
-- Returns an `AST` object containing a list of parsed expressions.
parseProgram :: AU.Parser AT.Program
parseProgram = fail "undefined"
