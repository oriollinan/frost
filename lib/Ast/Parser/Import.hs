module Ast.Parser.Import where

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

parseImport :: String -> PU.Parser String -> PU.Parser String
parseImport sourceFile p = do
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
          source' <- p
          S.modify $ PS.setImportDepth depth
          return source'
  where
    maxDepth = 25

localImport :: String -> String -> IO String
localImport sourceFile import' = readFile $ dir sourceFile ++ import'
  where
    dir = reverse . dropWhile (/= '/') . reverse

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
