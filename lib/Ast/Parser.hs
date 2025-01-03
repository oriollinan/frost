module Ast.Parser where

import qualified Ast.Parser.Env as E
import qualified Ast.Types as AT
import qualified Ast.Utils as AU
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a filename `String`, and an input `String` as a parameter and returns either an AST or an error message.
parse :: String -> String -> Either String AT.Program
parse filename input =
  case S.runState (M.runParserT parseProgram filename input) E.emptyEnv of
    (Left err, _) -> Left (M.errorBundlePretty err)
    (Right program, _) -> Right program

-- | Parses the top-level structure of a program.
-- Returns an `AST` object containing a list of parsed expressions.
parseProgram :: AU.Parser AT.Program
parseProgram = fail "undefined"
