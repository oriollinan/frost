module Ast.Parser.Import where

import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Utils as PU
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.State as S
import qualified Network.HTTP.Simple as N
import qualified Text.Megaparsec as M

parseImport :: PU.Parser String -> PU.Parser String
parseImport p = do
  import' <- PU.symbol "import" *> M.between (PU.symbol "\"") (PU.symbol "\"") (M.some M.anySingle)
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
          source <- case import' of
            ('.' : '/' : _) -> IO.liftIO $ localImport import'
            _ -> IO.liftIO $ externalImport import'
          input <- M.getInput
          M.setInput $ source ++ input
          source' <- p
          S.modify $ PS.setImportDepth depth
          return source'
  where
    maxDepth = 25

localImport :: String -> IO String
localImport = readFile

externalImport :: String -> IO String
externalImport url = do
  req <- case N.parseRequest url of
    Left err -> fail ("Invalid URL: " ++ show err)
    Right req -> return req
  res <- N.httpBS req
  return $ show $ N.getResponseBody res
