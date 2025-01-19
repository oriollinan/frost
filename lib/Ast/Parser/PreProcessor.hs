module Ast.Parser.PreProcessor where

import qualified Ast.Parser.PreProcessor.Define as PD
import qualified Ast.Parser.PreProcessor.Import as PI
import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Utils as PU
import qualified Control.Monad.State as S
import qualified Text.Megaparsec as M

-- | Preprocesses a source file by handling imports, defines, and other preprocessing tasks.
-- It recursively resolves imports and applies preprocessing rules to the source.
-- Returns the fully preprocessed source code as a `String`.
preprocess :: String -> PU.Parser String
preprocess sourceFile = do
  S.modify $ PS.insertImport sourceFile
  sources <-
    M.many $
      M.choice
        [ PI.parseImport sourceFile (preprocess sourceFile),
          PD.parseDefines,
          (: []) <$> M.anySingle
        ]
  return $ concat sources
