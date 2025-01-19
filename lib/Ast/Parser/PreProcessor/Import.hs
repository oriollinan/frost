module Ast.Parser.PreProcessor.Import where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Utils as PU
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State as S
import qualified Data.ByteString.Char8 as BS
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Simple as N
import qualified System.Environment as E
import qualified System.IO.Error as IOE
import qualified Text.Megaparsec as M

-- | Parses an import directive.
-- Handles both local and external imports while ensuring that circular imports and excessive depth are avoided.
--
-- Local imports are resolved relative to the current file's directory.
-- External imports fetch content over HTTP.
--
-- Returns the preprocessed source of the imported file.
parseImport :: String -> PU.Parser String -> PU.Parser String
parseImport sourceFile parser = do
  import' <- PU.symbol "import" *> M.between (PU.symbol "\"") (PU.symbol "\"") (M.some $ M.anySingleBut '\"')
  state <- S.get
  let visited = PS.lookupImport import' state
      depth = PS.getImportDepth state

  if visited
    then return ""
    else
      if depth >= maxDepth
        then fail "Maximum depth exceeded"
        else do
          S.modify $ PS.insertImport import'
          S.modify $ PS.setImportDepth $ depth + 1
          source <- IO.liftIO $ IOE.catchIOError (localImport sourceFile import') (\_ -> externalImport import')
          input <- M.getInput
          M.setInput $ source ++ input
          source' <- parser
          S.modify $ PS.setImportDepth depth
          return source'
  where
    maxDepth = 25

-- | Resolves a local import by reading a file relative to the current source file's directory.
--
-- Example:
-- If the `sourceFile` is "path/to/file" and the import is "importedFile",
-- this function reads "path/to/importedFile".
localImport :: String -> String -> IO String
localImport sourceFile import' = readFile $ dir sourceFile ++ import'
  where
    dir = reverse . dropWhile (/= '/') . reverse

-- | Resolves an external import by fetching content over HTTP.
--
-- If the environment variable `FROST_PRIVATE_REGISTRY_AUTH` is set, it adds a Bearer token for authorization.
--
-- Example:
-- Resolves "https://example.com/file" by making an HTTP GET request.
externalImport :: String -> IO String
externalImport url = do
  auth <- E.lookupEnv "FROST_PRIVATE_REGISTRY_AUTH"
  req <- case auth of
    Just token ->
      N.setRequestHeaders
        [(CI.mk $ BS.pack "Authorization", BS.pack $ "Bearer " ++ token)]
        <$> N.parseRequestThrow url
    Nothing -> N.parseRequestThrow url
  res <- N.httpBS req
  return $ BS.unpack $ N.getResponseBody res
