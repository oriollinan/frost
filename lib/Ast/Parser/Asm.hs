module Ast.Parser.Asm where

import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Data.Maybe as DM
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC

parseAsm :: PU.Parser AT.Expr -> PU.Parser AT.Type -> PU.Parser AT.AsmExpr
parseAsm ap tp = M.between (PU.symbol "{") (PU.symbol "}") $ do
  code <- PU.symbol "code ->" *> PU.lexeme anyString
  constraints <- PU.symbol "constraints ->" *> PU.lexeme parseAsmConstraint
  args <- PU.symbol "args ->" *> M.between (PU.symbol "(") (PU.symbol ")") (M.many ap)
  parameters <- PU.symbol "parameters ->" *> M.some (M.try tp)
  returnType <- PU.symbol "return_type ->" *> M.try tp
  sideEffects <- PU.symbol "side_effects ->" *> PU.lexeme PU.parseBool
  alignStack <- PU.symbol "align_stack ->" *> PU.lexeme PU.parseBool
  dialect <- PU.symbol "dialect ->" *> parseAsmDialect
  return $ AT.AsmExpr code constraints args parameters returnType sideEffects alignStack dialect

parseAsmConstraint :: PU.Parser AT.AsmConstraint
parseAsmConstraint = M.between (MC.char '"') (MC.char '"') $ do
  output <- M.optional parseConstraintOutput
  inputs <- M.optional $ sep *> M.sepBy parseConstraintInput sep
  return $ AT.AsmConstraint (DM.fromMaybe "" output) $ DM.fromMaybe [] inputs
  where
    sep = MC.string ","

parseConstraintInput :: PU.Parser String
parseConstraintInput = M.choice [MC.string "r", MC.string "m"]

parseConstraintOutput :: PU.Parser String
parseConstraintOutput = MC.char '=' *> M.choice [MC.string "r", MC.string "m"]

parseAsmDialect :: PU.Parser AT.AsmDialect
parseAsmDialect = M.choice [AT.Intel <$ PU.symbol "Intel", AT.ATT <$ PU.symbol "ATT"]

anyString :: PU.Parser String
anyString = M.between (MC.char '\"') (MC.char '\"') $ M.many PU.parseStringChar
