module Ast.Parser.AsmSpec (spec) where

import qualified Ast.Parser.Asm as PA
import qualified Ast.Parser.Expr as PE
import qualified Ast.Parser.State as PS
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parse input = do
        (result, _) <- S.runStateT (M.runParserT (PA.parseAsm PE.parseExpr) "" input) PS.parserState
        return result
  let normalizeAsm asm = asm {AT.asmArgs = map PU.normalizeExpr $ AT.asmArgs asm}

  describe "parseAsm" $ do
    it "parses a simple assembly expression" $ do
      let input = "{ code -> \"nop\" constraints -> \"\" args -> () side_effects -> false align_stack -> false }"
      result <- parse input
      let expected = Right $ AT.AsmExpr "nop" (AT.AsmConstraint "" []) [] False False
      result `shouldBe` expected

    it "parses a move expression" $ do
      let input = "{ code -> \"mov $0, 42\" constraints -> \"=r\" args -> () side_effects -> false align_stack -> false }"
      result <- parse input
      let expected = Right $ AT.AsmExpr "mov $0, 42" (AT.AsmConstraint "r" []) [] False False
      result `shouldBe` expected

    it "parses an add expression" $ do
      let input = "{ code -> \"add $0, $1\" constraints -> \"=r,r\" args -> (a b) side_effects -> false align_stack -> false }"
      result <- parse input
      let normalizedResult = normalizeAsm <$> result
      let expected = Right $ AT.AsmExpr "add $0, $1" (AT.AsmConstraint "r" ["r"]) [AT.Var PU.normalizeLoc "a" AT.TUnknown, AT.Var PU.normalizeLoc "b" AT.TUnknown] False False
      normalizedResult `shouldBe` expected

    it "parses an complex expression" $ do
      let input = "{ code -> \"mov $0, 0x1b; call printf\" constraints -> \"\" args -> (\"\x1b[H\") side_effects -> true align_stack -> false }"
      result <- parse input
      let normalizedResult = normalizeAsm <$> result
      let expected = Right $ AT.AsmExpr "mov $0, 0x1b; call printf" (AT.AsmConstraint "" []) [AT.Lit PU.normalizeLoc $ AT.LArray [AT.LChar '\x1b', AT.LChar '[', AT.LChar 'H']] True False
      normalizedResult `shouldBe` expected
