module Ast.Parser.ExprSpec (spec) where

import qualified Ast.Parser.Expr as PE
import qualified Ast.Parser.State as PS
import qualified Ast.Types as AT
import qualified Control.Monad.State as S
import Test.Hspec
import qualified Text.Megaparsec as M

spec :: Spec
spec = do
  let parse input = do
        (result, _) <- S.runStateT (M.runParserT PE.parseExpr "" input) PS.parserState
        return result
  let parseWithCustom env input = do
        (result, _) <- S.runStateT (M.runParserT PE.parseExpr "" input) env
        return result
  let parseMany input = do
        (result, _) <- S.runStateT (M.runParserT (M.many PE.parseExpr) "" input) PS.parserState
        return result

  describe "parseExpr" $ do
    it "parses a literal expression" $ do
      let input = "123"
      result <- parse input
      let expected = Right (AT.Lit normalizeLoc (AT.LInt 123))
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable expression" $
      do
        let input = "x"
        let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
        result <- parseWithCustom env input
        let expected = Right (AT.Var normalizeLoc "x" (AT.TInt 32))
        (normalizeExpr <$> result) `shouldBe` expected

    it "parses a snake case definition" $
      do
        let input = "x"
        let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
        result <- parseWithCustom env input
        let expected = Right (AT.Var normalizeLoc "x" (AT.TInt 32))
        (normalizeExpr <$> result) `shouldBe` expected

    it "unknown for undefined variable" $ do
      let input = "y"
      result <- parse input
      let expected = Right $ AT.Var normalizeLoc "y" AT.TUnknown
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a function declaration" $ do
      let input = "add: int int -> int = x y { ret 1 }"
      let expected =
            Right $
              AT.Function
                normalizeLoc
                "add"
                (AT.TFunction (AT.TInt 32) [AT.TInt 32, AT.TInt 32] False)
                ["x", "y"]
                (AT.Block [AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 1)))])
      result <- parse input
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a variadic function" $ do
      let input = "printf: *byte ... -> int = s { 1 }"
      let expected =
            Right $
              AT.Function
                normalizeLoc
                "printf"
                (AT.TFunction (AT.TInt 32) [AT.TPointer $ AT.TInt 8] True)
                ["s"]
                (AT.Block [AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 1)))])
      result <- parse input
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable declaration with initialization" $
      do
        let input = "x : int = 42"
        result <- parse input
        let expected =
              Right
                AT.Declaration
                  { AT.declLoc = AT.SrcLoc "" 0 0,
                    AT.declName = "x",
                    AT.declType = AT.TInt 32,
                    AT.declInit = Just (AT.Lit (AT.SrcLoc "" 0 00) (AT.LInt 42))
                  }
        (normalizeExpr <$> result) `shouldBe` expected

    it "parses a struct declaration with initialization" $
      do
        let input = "vector: Vector = Vector { x = 0 y = 0 }"
        let structType = AT.TStruct "Vector" [("x", AT.TInt 32), ("y", AT.TInt 32)]
        let env = PS.insertType "Vector" structType PS.parserState
        result <- parseWithCustom env input
        let expected =
              Right $
                AT.Declaration
                  normalizeLoc
                  "vector"
                  structType
                  ( Just
                      $ AT.Lit
                        normalizeLoc
                      $ AT.LStruct [("x", AT.LInt 0), ("y", AT.LInt 0)]
                  )
        (normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable declaration snake case" $
      do
        let input = "a_variable: int"
        result <- parse input
        let expected =
              Right $
                AT.Declaration
                  normalizeLoc
                  "a_variable"
                  ( AT.TInt
                      32
                  )
                  Nothing

        (normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable declaration with a custom int" $
      do
        let input = "x : int64 = 42"
        result <- parse input
        let expected =
              Right
                $ AT.Declaration
                  normalizeLoc
                  "x"
                  ( AT.TInt
                      64
                  )
                $ Just
                  (AT.Lit (AT.SrcLoc "" 0 00) (AT.LInt 42))
        (normalizeExpr <$> result) `shouldBe` expected

    it "parses an assignment expression" $ do
      let input = "x = 42"
      let env = PS.insertVar "x" (AT.TInt 0) PS.parserState
      result <- parseWithCustom env input
      let expected = Right (AT.Assignment normalizeLoc (AT.Var normalizeLoc "x" (AT.TInt 0)) (AT.Lit normalizeLoc (AT.LInt 42)))
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a function call" $ do
      let env = PS.insertVar "foo" (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False}) PS.parserState
      let input = "foo(123)"
      result <- parseWithCustom env input
      (normalizeExpr <$> result)
        `shouldBe` Right
          (AT.Call normalizeLoc (AT.Var normalizeLoc "foo" (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False})) [AT.Lit normalizeLoc (AT.LInt 123)])

    it "parses an if-else expression" $ do
      let input = "if x { ret 1 } else { ret 0 }"
      let env = PS.insertVar "x" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.If
                normalizeLoc
                (AT.Var normalizeLoc "x" AT.TBoolean)
                (AT.Block [AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 1)))])
                (Just (AT.Block [AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 0)))]))

      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an if-else expression with implicit returns" $ do
      let input = "main: never -> int = { if x { 1 } else { 0 } }"
      let env = PS.insertVar "x" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Function
                normalizeLoc
                "main"
                (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
                []
                ( AT.Block
                    [ AT.If
                        normalizeLoc
                        (AT.Var normalizeLoc "x" AT.TBoolean)
                        (AT.Block [AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 1)))])
                        (Just (AT.Block [AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 0)))]))
                    ]
                )

      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an void function without implicit returns" $ do
      let input = "main: never -> never = { if x { 1 } else { 0 } }"
      let env = PS.insertVar "x" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Function
                normalizeLoc
                "main"
                (AT.TFunction AT.TVoid [AT.TVoid] False)
                []
                ( AT.Block
                    [ AT.If
                        normalizeLoc
                        (AT.Var normalizeLoc "x" AT.TBoolean)
                        (AT.Block [AT.Lit normalizeLoc (AT.LInt 1)])
                        (Just $ AT.Block [AT.Lit normalizeLoc (AT.LInt 0)])
                    ]
                )

      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a while loop" $ do
      let input = "loop z { z = 0 }"
      let env = PS.insertVar "z" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.While
                normalizeLoc
                ( AT.Var
                    normalizeLoc
                    "z"
                    (AT.TInt 32)
                )
                ( AT.Block
                    [ AT.Assignment
                        normalizeLoc
                        (AT.Var normalizeLoc "z" (AT.TInt 32))
                        (AT.Lit normalizeLoc (AT.LInt 0))
                    ]
                )
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a for loop" $ do
      let input = "from 0 to 10 by 2 |i: int| { i = 0 }"
      let env = PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.For
                { AT.forLoc = normalizeLoc,
                  AT.forInit =
                    AT.Declaration
                      normalizeLoc
                      "i"
                      (AT.TInt 32)
                      (Just (AT.Lit normalizeLoc (AT.LInt 0))),
                  AT.forCond =
                    AT.Op
                      normalizeLoc
                      AT.Lt
                      (AT.Var normalizeLoc "i" (AT.TInt 32))
                      (AT.Lit normalizeLoc (AT.LInt 10)),
                  AT.forStep =
                    AT.Assignment
                      normalizeLoc
                      (AT.Var normalizeLoc "i" (AT.TInt 32))
                      ( AT.Op
                          normalizeLoc
                          AT.Add
                          (AT.Var normalizeLoc "i" (AT.TInt 32))
                          (AT.Lit normalizeLoc (AT.LInt 2))
                      ),
                  AT.forBody =
                    AT.Block
                      [ AT.Assignment
                          normalizeLoc
                          (AT.Var normalizeLoc "i" (AT.TInt 32))
                          ( AT.Lit normalizeLoc (AT.LInt 0)
                          )
                      ]
                }
      (normalizeExpr <$> result) `shouldBe` expected

    -- it "parses a for loop with a dynamic range" $ do
    --   let input = "from 0 to 10 by x |i: int| { i = 0 }"
    --   let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
    --   result <- parseWithCustom env input
    --   let expected =
    --         Right $
    --           AT.For
    --             { AT.forLoc = normalizeLoc,
    --               AT.forInit =
    --                 AT.Declaration
    --                   normalizeLoc
    --                   "i"
    --                   (AT.TInt 32)
    --                   (Just (AT.Lit normalizeLoc (AT.LInt 0))),
    --               AT.forCond =
    --                 AT.Op
    --                   normalizeLoc
    --                   AT.Lt
    --                   (AT.Var normalizeLoc "i" (AT.TInt 32))
    --                   (AT.Lit normalizeLoc (AT.LInt 10)),
    --               AT.forStep =
    --                 AT.Assignment
    --                   normalizeLoc
    --                   (AT.Var normalizeLoc "i" (AT.TInt 32))
    --                   ( AT.Op
    --                       normalizeLoc
    --                       AT.Add
    --                       (AT.Var normalizeLoc "i" (AT.TInt 32))
    --                       (AT.Var normalizeLoc "x" (AT.TInt 32))
    --                   ),
    --               AT.forBody =
    --                 AT.Block
    --                   [ AT.Assignment
    --                       normalizeLoc
    --                       (AT.Var normalizeLoc "i" (AT.TInt 32))
    --                       ( AT.Lit normalizeLoc (AT.LInt 0)
    --                       )
    --                   ]
    --             }
    --   (normalizeExpr <$> result) `shouldBe` expected

    it "parses a break statement" $ do
      let input = "stop"
      result <- parse input
      let expected = Right (AT.Break normalizeLoc)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a continue statement" $ do
      let input = "next"
      result <- parse input
      let expected = Right (AT.Continue normalizeLoc)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a struct access" $ do
      let input = "myStruct.myField"
      let structType = AT.TStruct "Custom" [("myField", AT.TChar)]
      let env = PS.insertVar "myStruct" structType $ PS.insertType "Custom" structType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.StructAccess
                normalizeLoc
                (AT.Var normalizeLoc "myStruct" structType)
                (AT.Var normalizeLoc "myField" AT.TUnknown)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a nested struct access" $ do
      let input = "myStruct.innerStruct.field"
      let structType = AT.TStruct "Custom" [("innerStruct", AT.TStruct "InnerCustom" [("field", AT.TChar)])]
      let env = PS.insertVar "myStruct" structType $ PS.insertType "Custom" structType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.StructAccess
                normalizeLoc
                ( AT.StructAccess
                    normalizeLoc
                    (AT.Var normalizeLoc "myStruct" structType)
                    (AT.Var normalizeLoc "innerStruct" AT.TUnknown)
                )
                (AT.Var normalizeLoc "field" AT.TUnknown)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an array access" $ do
      let input = "myArray.#1"
      let arrayType = AT.TArray AT.TChar Nothing
      let env = PS.insertVar "myArray" arrayType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.ArrayAccess
                normalizeLoc
                (AT.Var normalizeLoc "myArray" arrayType)
                (AT.Lit normalizeLoc $ AT.LInt 1)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an nested array access" $ do
      let input = "myArray.#1.#1"
      let arrayType = AT.TArray (AT.TArray AT.TChar Nothing) Nothing
      let env = PS.insertVar "myArray" arrayType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.ArrayAccess
                normalizeLoc
                ( AT.ArrayAccess
                    normalizeLoc
                    (AT.Var normalizeLoc "myArray" arrayType)
                    (AT.Lit normalizeLoc $ AT.LInt 1)
                )
                (AT.Lit normalizeLoc $ AT.LInt 1)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an type cast" $ do
      let input = "@int('a')"
      result <- parse input
      let expected =
            Right $
              AT.Cast
                normalizeLoc
                (AT.TInt 32)
                (AT.Lit normalizeLoc $ AT.LChar 'a')
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an operator" $ do
      let input = "1 + 1"
      result <- parse input
      let expected =
            Right $
              AT.Op
                normalizeLoc
                AT.Add
                (AT.Lit normalizeLoc $ AT.LInt 1)
                (AT.Lit normalizeLoc $ AT.LInt 1)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an operator with hierarchy" $ do
      let input = "1 + 1 * 2"
      result <- parse input
      let expected =
            Right $
              AT.Op
                normalizeLoc
                AT.Add
                (AT.Lit normalizeLoc $ AT.LInt 1)
                ( AT.Op
                    normalizeLoc
                    AT.Mul
                    (AT.Lit normalizeLoc $ AT.LInt 1)
                    (AT.Lit normalizeLoc $ AT.LInt 2)
                )
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses an operator with hierarchy and comparisons" $ do
      let input = "n is 0 or n is 1"
      let env = PS.insertVar "n" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Op
                normalizeLoc
                AT.Or
                ( AT.Op
                    normalizeLoc
                    AT.Eq
                    (AT.Var normalizeLoc "n" $ AT.TInt 32)
                    (AT.Lit normalizeLoc $ AT.LInt 0)
                )
                ( AT.Op
                    normalizeLoc
                    AT.Eq
                    (AT.Var normalizeLoc "n" $ AT.TInt 32)
                    (AT.Lit normalizeLoc $ AT.LInt 1)
                )
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a unary operator" $ do
      let input = "not 1"
      result <- parse input
      let expected =
            Right $
              AT.UnaryOp
                normalizeLoc
                AT.Not
                (AT.Lit normalizeLoc $ AT.LInt 1)

      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a line comment" $ do
      let input = "not 1 % this is a line comment"
      result <- parse input
      let expected =
            Right $
              AT.UnaryOp
                normalizeLoc
                AT.Not
                (AT.Lit normalizeLoc $ AT.LInt 1)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a block comment" $ do
      let input = "not 1 %% this is a block comment %%"
      result <- parse input
      let expected =
            Right $
              AT.UnaryOp
                normalizeLoc
                AT.Not
                (AT.Lit normalizeLoc $ AT.LInt 1)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a higher order function" $ do
      let input = "map: (int -> char) int -> char = f x { f(x) }"
      let functionType = AT.TFunction AT.TChar [AT.TInt 32] False
      result <- parse input
      let expected =
            Right $
              AT.Function
                normalizeLoc
                "map"
                (AT.TFunction AT.TChar [functionType, AT.TInt 32] False)
                ["f", "x"]
                ( AT.Block
                    [ AT.Return normalizeLoc $ Just $ AT.Call normalizeLoc (AT.Var normalizeLoc "f" functionType) [AT.Var normalizeLoc "x" $ AT.TInt 32]
                    ]
                )
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a prefix operator" $ do
      let input = "++x"
      let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.UnaryOp normalizeLoc AT.PreInc (AT.Var normalizeLoc "x" $ AT.TInt 32)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a postfix operator" $ do
      let input = "x++"
      let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.UnaryOp normalizeLoc AT.PostInc (AT.Var normalizeLoc "x" $ AT.TInt 32)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a deref operator" $ do
      let input = "x.*"
      let varType = AT.TPointer $ AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.UnaryOp normalizeLoc AT.Deref (AT.Var normalizeLoc "x" varType)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a deref assignment" $ do
      let input = "x.* = 1"
      let varType = AT.TPointer $ AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Assignment
                normalizeLoc
                (AT.UnaryOp normalizeLoc AT.Deref (AT.Var normalizeLoc "x" varType))
                (AT.Lit normalizeLoc $ AT.LInt 1)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a foreign function" $ do
      let input = "print: foreign int -> never"
      result <- parse input
      let expected =
            Right $
              AT.ForeignFunction
                normalizeLoc
                "print"
                (AT.TFunction AT.TVoid [AT.TInt 32] False)
      (normalizeExpr <$> result) `shouldBe` expected

    it "parses a foreign function with a function declaration after it" $
      do
        let input = "free: foreign *byte -> never\nmain: never -> int = { free(0) 0 }"
        result <- parseMany input
        let expected =
              Right $
                AT.Block
                  [ AT.ForeignFunction
                      normalizeLoc
                      "free"
                      (AT.TFunction AT.TVoid [AT.TPointer (AT.TInt 8)] False),
                    AT.Function
                      normalizeLoc
                      "main"
                      (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
                      []
                      ( AT.Block
                          [ AT.Call
                              normalizeLoc
                              (AT.Var normalizeLoc "free" (AT.TFunction AT.TVoid [AT.TPointer (AT.TInt 8)] False))
                              [AT.Lit normalizeLoc (AT.LInt 0)],
                            AT.Return normalizeLoc (Just (AT.Lit normalizeLoc (AT.LInt 0)))
                          ]
                      )
                  ]
        (normalizeExpr . AT.Block <$> result) `shouldBe` expected

normalizeLoc :: AT.SrcLoc
normalizeLoc = AT.SrcLoc "" 0 0

normalizeExpr :: AT.Expr -> AT.Expr
normalizeExpr (AT.Lit _ lit) = AT.Lit normalizeLoc lit
normalizeExpr (AT.Var _ name t) = AT.Var normalizeLoc name t
normalizeExpr (AT.Function _ name t params body) = AT.Function normalizeLoc name t params (normalizeExpr body)
normalizeExpr (AT.Declaration _ name t initVal) = AT.Declaration normalizeLoc name t (fmap normalizeExpr initVal)
normalizeExpr (AT.Assignment _ target value) = AT.Assignment normalizeLoc (normalizeExpr target) (normalizeExpr value)
normalizeExpr (AT.Call _ func args) = AT.Call normalizeLoc (normalizeExpr func) (map normalizeExpr args)
normalizeExpr (AT.If _ cond thenBranch elseBranch) = AT.If normalizeLoc (normalizeExpr cond) (normalizeExpr thenBranch) (fmap normalizeExpr elseBranch)
normalizeExpr (AT.Block exprs) = AT.Block (map normalizeExpr exprs)
normalizeExpr (AT.Return _ value) = AT.Return normalizeLoc (fmap normalizeExpr value)
normalizeExpr (AT.Op _ op e1 e2) = AT.Op normalizeLoc op (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.UnaryOp _ op e) = AT.UnaryOp normalizeLoc op (normalizeExpr e)
normalizeExpr (AT.For _ i c s b) = AT.For normalizeLoc (normalizeExpr i) (normalizeExpr c) (normalizeExpr s) (normalizeExpr b)
normalizeExpr (AT.While _ c b) = AT.While normalizeLoc (normalizeExpr c) (normalizeExpr b)
normalizeExpr (AT.Continue _) = AT.Continue normalizeLoc
normalizeExpr (AT.Break _) = AT.Break normalizeLoc
normalizeExpr (AT.StructAccess _ e1 e2) = AT.StructAccess normalizeLoc (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.ArrayAccess _ e1 e2) = AT.ArrayAccess normalizeLoc (normalizeExpr e1) (normalizeExpr e2)
normalizeExpr (AT.Cast _ t e) = AT.Cast normalizeLoc t (normalizeExpr e)
normalizeExpr (AT.ForeignFunction _ n t) = AT.ForeignFunction normalizeLoc n t
