module Ast.Parser.UnaryOperationSpec where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.UnaryOperation as AUO
import qualified Ast.Parser.Utils as PU
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Data.Either (isLeft)
import Test.Hspec
import qualified Text.Megaparsec as M

-- Assuming PU.identifier parses an identifier like "x"
-- If not, you can define a simple operand parser as follows:
-- operandParser :: PU.Parser String
-- operandParser = PU.symbol "x" >> return "x"

spec :: Spec
spec = do
  let initialEnv = E.emptyEnv

  -- Define an operand parser. Replace PU.identifier with your actual identifier parser.
  let operandParser = PU.identifier

  -- Helper function to run the parser with the initial environment
  let parseWithEnv parser input =
        S.runState (M.runParserT parser "" input) initialEnv

  describe "parseUnaryOperation" $ do
    context "Pre-unary operators" $ do
      it "parses logical NOT '!x'" $ do
        let input = "!x"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.Not, "x"), initialEnv)

      it "parses 'notx'" $ do
        let input = "notx"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.Not, "x"), initialEnv)

      it "parses bitwise NOT '~x'" $ do
        let input = "~x"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.BitNot, "x"), initialEnv)

      it "parses address-of operator '&x'" $ do
        let input = "&x"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.AddrOf, "x"), initialEnv)

      it "parses pre-unary increment '++x'" $ do
        let input = "++x"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.PreInc, "x"), initialEnv)

      it "parses pre-unary decrement '--x'" $ do
        let input = "--x"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.PreDec, "x"), initialEnv)

    context "Post-unary operators" $ do
      it "parses dereference operator 'x.'" $ do
        let input = "x."
            parser = AUO.parsePostUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.Deref, "x"), initialEnv)

      it "parses post-unary increment 'x++'" $ do
        let input = "x++"
            parser = AUO.parsePostUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.PostInc, "x"), initialEnv)

      it "parses post-unary decrement 'x--'" $ do
        let input = "x--"
            parser = AUO.parsePostUnaryOperation operandParser
            result = parseWithEnv parser input
        result `shouldBe` (Right (AT.PostDec, "x"), initialEnv)

    context "Invalid operators" $ do
      it "returns error for invalid pre-unary operator 'invalidx'" $ do
        let input = "invalidx"
            parser = AUO.parsePreUnaryOperation operandParser
            result = parseWithEnv parser input
        fst result `shouldSatisfy` isLeft

      it "returns error for invalid post-unary operator 'xinvalid'" $ do
        let input = "xinvalid"
            parser = AUO.parsePostUnaryOperation operandParser
            result = parseWithEnv parser input
        fst result `shouldSatisfy` isLeft
