module Ast.Parser.PreProcessor.Define where

import qualified Ast.Parser.Utils as PU
import qualified Control.Monad.IO.Class as IO
import qualified Data.Maybe as DM
import qualified System.Environment.Blank as EB
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

-- | Parses define directives.
-- Supports conditional logic based on environment variables.
-- Returns the processed string after applying defines.
parseDefines :: PU.Parser String
parseDefines = M.choice [M.try parseDefine, parseSet]

-- | Parses a `?defined` directive.
-- The directive checks if an environment variable is defined.
-- If the variable exists, the `thenBlock` is returned.
-- Otherwise, the `elseBlock` (if present) is returned.
parseDefine :: PU.Parser String
parseDefine = do
  varName <- MC.string "?defined" *> M.between (MC.string "(") (MC.string ")") (M.takeWhileP Nothing (/= ')'))
  thenBlock <- M.takeWhileP Nothing (/= '?')
  elseBlock <- M.optional $ MC.string "?else" *> M.takeWhileP Nothing (/= '?')
  _ <- MC.string "?end"

  envVar <- IO.liftIO $ EB.getEnv varName
  case envVar of
    Just _ -> return thenBlock
    _ -> return $ DM.fromMaybe "" elseBlock

-- | Parses a `?set` directive.
-- The directive sets an environment variable to an empty value.
parseSet :: PU.Parser String
parseSet = do
  varName <- MC.string "?set" *> M.between (MC.string "(") (MC.string ")") (M.takeWhileP Nothing (/= ')'))
  _ <- IO.liftIO $ EB.setEnv varName "" False
  return ""
