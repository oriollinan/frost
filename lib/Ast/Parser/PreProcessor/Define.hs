module Ast.Parser.PreProcessor.Define where

import qualified Ast.Parser.Utils as PU
import qualified Control.Monad.IO.Class as IO
import qualified Data.Maybe as DM
import qualified System.Environment.Blank as EB
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

parseDefines :: PU.Parser String
parseDefines = M.choice [M.try parseDefine, parseSet]

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

parseSet :: PU.Parser String
parseSet = do
  varName <- MC.string "?set" *> M.between (MC.string "(") (MC.string ")") (M.takeWhileP Nothing (/= ')'))
  _ <- IO.liftIO $ EB.setEnv varName "" False
  return ""
