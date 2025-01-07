module Ast.Parser.ExprSpec (spec) where

import qualified Ast.Parser.Env as E
import qualified Ast.Parser.Expr as PE
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let initialEnv = E.emptyEnv
  let parseWithEnv input =
        fst $ S.runState (M.runParserT PE.parseExpr "" input) initialEnv

  describe "parseExpr" $ do
    it "parses a literal expression" $ do
      parseWithEnv "123" `shouldBe` Right (AT.Lit (AT.SrcLoc "" 1 4) (AT.LInt 123))

    it "parses a variable expression" $ do
      let env = E.insertVar "x" (AT.TInt 0) initialEnv
      fst (S.runState (M.runParserT PE.parseExpr "" "x") env)
        `shouldBe` Right (AT.Var (AT.SrcLoc "" 1 2) "x" (AT.TInt 0))

    it "fails for an undefined variable" $ do
      let result = parseWithEnv "y"
      case result of
        Left _ -> True `shouldBe` True
        _ -> error "Expected failure"

    it "parses a function declaration" $ do
      let input = "add : (int int) -> (int) = x y { return 1 }"
      let expected =
            AT.Function
              (AT.SrcLoc "" 0 0)
              "add"
              (AT.TFunction {AT.returnType = AT.TInt 32, AT.paramTypes = [AT.TInt 32, AT.TInt 32], AT.isVariadic = False})
              ["x", "y"]
              (AT.Block [AT.Return (AT.SrcLoc "" 0 0) (Just (AT.Lit (AT.SrcLoc "" 0 0) (AT.LInt 1)))])
      let result = normalizeExpr <$> parseWithEnv input
      result `shouldBe` Right (normalizeExpr expected)

    it "parses a variable declaration with initialization" $ do
      let input = "x : int = 42"
      let expected =
            AT.Declaration
              { AT.declLoc = AT.SrcLoc "" 0 0,
                AT.declName = "x",
                AT.declType = AT.TInt 0,
                AT.declInit = Just (AT.Lit (AT.SrcLoc "" 0 00) (AT.LInt 42))
              }
      normalizeExpr <$> parseWithEnv input `shouldBe` Right (normalizeExpr expected)

    it "parses an assignment expression" $ do
      let input = "x = 42"
      let env = E.insertVar "x" (AT.TInt 0) initialEnv
      normalizeExpr <$> fst (S.runState (M.runParserT PE.parseExpr "" input) env)
        `shouldBe` Right (AT.Assignment (AT.SrcLoc "" 0 0) (AT.Var (AT.SrcLoc "" 0 0) "x" (AT.TInt 0)) (AT.Lit (AT.SrcLoc "" 0 0) (AT.LInt 42)))

    it "parses a function call" $ do
      let env = E.insertVar "foo" (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False}) initialEnv
      let input = "foo(123)"
      normalizeExpr <$> fst (S.runState (M.runParserT PE.parseExpr "" input) env)
        `shouldBe` Right
          (AT.Call (AT.SrcLoc "" 0 0) (AT.Var (AT.SrcLoc "" 0 0) "foo" (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False})) [AT.Lit (AT.SrcLoc "" 0 0) (AT.LInt 123)])

    it "parses an if-else expression" $ do
      let input = "if x { return 1 } else { return 0 }"
      let env = E.insertVar "x" AT.TBoolean initialEnv
      normalizeExpr <$> fst (S.runState (M.runParserT PE.parseExpr "" input) env)
        `shouldBe` Right
          ( AT.If
              (AT.SrcLoc "" 0 0)
              (AT.Var (AT.SrcLoc "" 0 0) "x" AT.TBoolean)
              (AT.Block [AT.Return (AT.SrcLoc "" 0 0) (Just (AT.Lit (AT.SrcLoc "" 0 0) (AT.LInt 1)))])
              (Just (AT.Block [AT.Return (AT.SrcLoc "" 0 0) (Just (AT.Lit (AT.SrcLoc "" 0 0) (AT.LInt 0)))]))
          )

normalizeLoc :: AT.SrcLoc
normalizeLoc = AT.SrcLoc "" 0 0

normalizeExpr :: AT.Expr -> AT.Expr
normalizeExpr (AT.Lit _ lit) = AT.Lit normalizeLoc lit
normalizeExpr (AT.Var _ name t) = AT.Var normalizeLoc name t
normalizeExpr (AT.Function _ name t params body) =
  AT.Function normalizeLoc name t params (normalizeExpr body)
normalizeExpr (AT.Declaration _ name t initVal) =
  AT.Declaration normalizeLoc name t (fmap normalizeExpr initVal)
normalizeExpr (AT.Assignment _ target value) =
  AT.Assignment normalizeLoc (normalizeExpr target) (normalizeExpr value)
normalizeExpr (AT.Call _ func args) =
  AT.Call normalizeLoc (normalizeExpr func) (map normalizeExpr args)
normalizeExpr (AT.If _ cond thenBranch elseBranch) =
  AT.If normalizeLoc (normalizeExpr cond) (normalizeExpr thenBranch) (fmap normalizeExpr elseBranch)
normalizeExpr (AT.Block exprs) = AT.Block (map normalizeExpr exprs)
normalizeExpr (AT.Return _ value) = AT.Return normalizeLoc (fmap normalizeExpr value)
normalizeExpr (AT.Op _ op e1 e2) =
  AT.Op normalizeLoc op (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.UnaryOp _ op e) =
  AT.UnaryOp normalizeLoc op (normalizeExpr e)
normalizeExpr (AT.For _ i c s b) = AT.For normalizeLoc i c s b
normalizeExpr (AT.While _ c b) = AT.While normalizeLoc c b
normalizeExpr (AT.Continue _) = AT.Continue normalizeLoc
normalizeExpr (AT.Break _) = AT.Break normalizeLoc
normalizeExpr (AT.StructAccess _ e s) = AT.StructAccess normalizeLoc e s
normalizeExpr (AT.ArrayAccess _ e1 e2) = AT.ArrayAccess normalizeLoc e1 e2
normalizeExpr (AT.Cast _ t e) = AT.Cast normalizeLoc t e
