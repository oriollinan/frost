module Ast.Parser where

import qualified Ast.Parser.Program as PP
import qualified Ast.Parser.State as PS
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

-- | Parses a string into an abstract syntax tree (AST).
-- The `parse` function takes a filename `String`, and an input `String` as a parameter and returns either an AST or an error message.
parse :: String -> String -> IO (Either String AT.Program)
parse sourceFile input = do
  (result, _) <- S.runStateT (M.runParserT (PP.parseProgram sourceFile) sourceFile input) PS.parserState
  case result of
    (Left err) -> return $ Left (M.errorBundlePretty err)
    (Right program) -> return $ Right program
