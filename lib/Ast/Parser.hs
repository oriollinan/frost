module Ast.Parser where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Program as PP
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a filename `String`, and an input `String` as a parameter and returns either an AST or an error message.
parse :: String -> String -> Either String AT.Program
parse sourceFile input =
  case S.runState (M.runParserT (PP.parseProgram sourceFile) sourceFile input) E.emptyEnv of
    (Left err, _) -> Left (M.errorBundlePretty err)
    (Right program, _) -> Right program
