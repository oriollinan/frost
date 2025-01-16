module Ast.Parser.ExprSpec (spec) where

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
      let expected = Right (AT.Lit PU.normalizeLoc (AT.LInt 123))
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable expression" $
      do
        let input = "x"
        let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
        result <- parseWithCustom env input
        let expected = Right (AT.Var PU.normalizeLoc "x" (AT.TInt 32))
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a snake case definition" $
      do
        let input = "x"
        let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
        result <- parseWithCustom env input
        let expected = Right (AT.Var PU.normalizeLoc "x" (AT.TInt 32))
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "unknown for undefined variable" $ do
      let input = "y"
      result <- parse input
      let expected = Right $ AT.Var PU.normalizeLoc "y" AT.TUnknown
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a function declaration" $ do
      let input = "add: int int -> int = x y { ret 1 }"
      let expected =
            Right $
              AT.Function
                PU.normalizeLoc
                "add"
                (AT.TFunction (AT.TInt 32) [AT.TInt 32, AT.TInt 32] False)
                ["x", "y"]
                (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 1)))])
      result <- parse input
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a variadic function" $ do
      let input = "printf: *byte ... -> int = s { 1 }"
      let expected =
            Right $
              AT.Function
                PU.normalizeLoc
                "printf"
                (AT.TFunction (AT.TInt 32) [AT.TPointer $ AT.TInt 8] True)
                ["s"]
                (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 1)))])
      result <- parse input
      (PU.normalizeExpr <$> result) `shouldBe` expected

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
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a struct declaration with initialization" $
      do
        let input = "vector: Vector = Vector { x = 0 y = 0 }"
        let structType = AT.TStruct "Vector" [("x", AT.TInt 32), ("y", AT.TInt 32)]
        let env = PS.insertType "Vector" structType PS.parserState
        result <- parseWithCustom env input
        let expected =
              Right $
                AT.Declaration
                  PU.normalizeLoc
                  "vector"
                  structType
                  ( Just
                      $ AT.Lit
                        PU.normalizeLoc
                      $ AT.LStruct [("x", AT.LInt 0), ("y", AT.LInt 0)]
                  )
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable declaration snake case" $
      do
        let input = "a_variable: int"
        result <- parse input
        let expected =
              Right $
                AT.Declaration
                  PU.normalizeLoc
                  "a_variable"
                  ( AT.TInt
                      32
                  )
                  Nothing

        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a variable declaration with a custom int" $
      do
        let input = "x : int64 = 42"
        result <- parse input
        let expected =
              Right
                $ AT.Declaration
                  PU.normalizeLoc
                  "x"
                  ( AT.TInt
                      64
                  )
                $ Just
                  (AT.Lit (AT.SrcLoc "" 0 00) (AT.LInt 42))
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an assignment expression" $ do
      let input = "x = 42"
      let env = PS.insertVar "x" (AT.TInt 0) PS.parserState
      result <- parseWithCustom env input
      let expected = Right (AT.Assignment PU.normalizeLoc (AT.Var PU.normalizeLoc "x" (AT.TInt 0)) (AT.Lit PU.normalizeLoc (AT.LInt 42)))
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a function call" $ do
      let env = PS.insertVar "foo" (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False}) PS.parserState
      let input = "foo(123)"
      result <- parseWithCustom env input
      (PU.normalizeExpr <$> result)
        `shouldBe` Right
          (AT.Call PU.normalizeLoc (AT.Var PU.normalizeLoc "foo" (AT.TFunction {AT.returnType = AT.TVoid, AT.paramTypes = [AT.TInt 0], AT.isVariadic = False})) [AT.Lit PU.normalizeLoc (AT.LInt 123)])

    it "parses an if-else expression" $ do
      let input = "if x { ret 1 } else { ret 0 }"
      let env = PS.insertVar "x" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.If
                PU.normalizeLoc
                (AT.Var PU.normalizeLoc "x" AT.TBoolean)
                (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 1)))])
                (Just (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 0)))]))

      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an if-else expression with implicit returns" $ do
      let input = "main: never -> int = { if x { 1 } else { 0 } }"
      let env = PS.insertVar "x" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Function
                PU.normalizeLoc
                "main"
                (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
                []
                ( AT.Block
                    [ AT.If
                        PU.normalizeLoc
                        (AT.Var PU.normalizeLoc "x" AT.TBoolean)
                        (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 1)))])
                        (Just (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 0)))]))
                    ]
                )

      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an void function without implicit returns" $ do
      let input = "main: never -> never = { if x { 1 } else { 0 } }"
      let env = PS.insertVar "x" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Function
                PU.normalizeLoc
                "main"
                (AT.TFunction AT.TVoid [AT.TVoid] False)
                []
                ( AT.Block
                    [ AT.If
                        PU.normalizeLoc
                        (AT.Var PU.normalizeLoc "x" AT.TBoolean)
                        (AT.Block [AT.Lit PU.normalizeLoc (AT.LInt 1)])
                        (Just $ AT.Block [AT.Lit PU.normalizeLoc (AT.LInt 0)])
                    ]
                )

      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a while loop" $ do
      let input = "loop z { z = 0 }"
      let env = PS.insertVar "z" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.While
                PU.normalizeLoc
                ( AT.Var
                    PU.normalizeLoc
                    "z"
                    (AT.TInt 32)
                )
                ( AT.Block
                    [ AT.Assignment
                        PU.normalizeLoc
                        (AT.Var PU.normalizeLoc "z" (AT.TInt 32))
                        (AT.Lit PU.normalizeLoc (AT.LInt 0))
                    ]
                )
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a for loop" $ do
      let input = "from 0 to 10 by 2 [i: int] { i = 0 }"
      let env = PS.parserState
      result <- parseWithCustom env input
      let startValue = AT.Lit PU.normalizeLoc $ AT.LInt 0
      let var = AT.Var PU.normalizeLoc "i" $ AT.TInt 32
      let expected =
            Right $
              AT.From
                PU.normalizeLoc
                startValue
                (AT.Lit PU.normalizeLoc $ AT.LInt 10)
                ( AT.Assignment PU.normalizeLoc var $
                    AT.Op
                      PU.normalizeLoc
                      AT.Add
                      var
                      (AT.Lit PU.normalizeLoc $ AT.LInt 2)
                )
                (AT.Declaration PU.normalizeLoc "i" (AT.TInt 32) $ Just startValue)
                ( AT.Block
                    [ AT.Assignment
                        PU.normalizeLoc
                        (AT.Var PU.normalizeLoc "i" (AT.TInt 32))
                        ( AT.Lit PU.normalizeLoc (AT.LInt 0)
                        )
                    ]
                )
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a for loop with a dynamic range" $ do
      let input = "from 0 to 10 by x [i: int] { i = 0 }"
      let varI = AT.Var PU.normalizeLoc "i" (AT.TInt 32)
      let varX = AT.Var PU.normalizeLoc "x" (AT.TInt 32)
      let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let startValue = AT.Lit PU.normalizeLoc $ AT.LInt 0
      let expected =
            Right $
              AT.From
                PU.normalizeLoc
                startValue
                (AT.Lit PU.normalizeLoc $ AT.LInt 10)
                ( AT.Assignment PU.normalizeLoc varI $
                    AT.Op
                      PU.normalizeLoc
                      AT.Add
                      varI
                      varX
                )
                (AT.Declaration PU.normalizeLoc "i" (AT.TInt 32) $ Just startValue)
                ( AT.Block
                    [ AT.Assignment
                        PU.normalizeLoc
                        (AT.Var PU.normalizeLoc "i" (AT.TInt 32))
                        ( AT.Lit PU.normalizeLoc (AT.LInt 0)
                        )
                    ]
                )
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a break statement" $ do
      let input = "stop"
      result <- parse input
      let expected = Right (AT.Break PU.normalizeLoc)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a continue statement" $ do
      let input = "next"
      result <- parse input
      let expected = Right (AT.Continue PU.normalizeLoc)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a struct access" $ do
      let input = "myStruct.myField"
      let structType = AT.TStruct "Custom" [("myField", AT.TChar)]
      let env = PS.insertVar "myStruct" structType $ PS.insertType "Custom" structType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.StructAccess
                PU.normalizeLoc
                (AT.Var PU.normalizeLoc "myStruct" structType)
                (AT.Var PU.normalizeLoc "myField" AT.TUnknown)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a nested struct access" $ do
      let input = "myStruct.innerStruct.field"
      let structType = AT.TStruct "Custom" [("innerStruct", AT.TStruct "InnerCustom" [("field", AT.TChar)])]
      let env = PS.insertVar "myStruct" structType $ PS.insertType "Custom" structType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.StructAccess
                PU.normalizeLoc
                ( AT.StructAccess
                    PU.normalizeLoc
                    (AT.Var PU.normalizeLoc "myStruct" structType)
                    (AT.Var PU.normalizeLoc "innerStruct" AT.TUnknown)
                )
                (AT.Var PU.normalizeLoc "field" AT.TUnknown)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an array access" $ do
      let input = "myArray.#1"
      let arrayType = AT.TArray AT.TChar Nothing
      let env = PS.insertVar "myArray" arrayType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.ArrayAccess
                PU.normalizeLoc
                (AT.Var PU.normalizeLoc "myArray" arrayType)
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an nested array access" $ do
      let input = "myArray.#1.#1"
      let arrayType = AT.TArray (AT.TArray AT.TChar Nothing) Nothing
      let env = PS.insertVar "myArray" arrayType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.ArrayAccess
                PU.normalizeLoc
                ( AT.ArrayAccess
                    PU.normalizeLoc
                    (AT.Var PU.normalizeLoc "myArray" arrayType)
                    (AT.Lit PU.normalizeLoc $ AT.LInt 1)
                )
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an type cast" $ do
      let input = "@int('a')"
      result <- parse input
      let expected =
            Right $
              AT.Cast
                PU.normalizeLoc
                (AT.TInt 32)
                (AT.Lit PU.normalizeLoc $ AT.LChar 'a')
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an operator" $ do
      let input = "1 + 1"
      result <- parse input
      let expected =
            Right $
              AT.Op
                PU.normalizeLoc
                AT.Add
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an operator with hierarchy" $ do
      let input = "1 + 1 * 2"
      result <- parse input
      let expected =
            Right $
              AT.Op
                PU.normalizeLoc
                AT.Add
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
                ( AT.Op
                    PU.normalizeLoc
                    AT.Mul
                    (AT.Lit PU.normalizeLoc $ AT.LInt 1)
                    (AT.Lit PU.normalizeLoc $ AT.LInt 2)
                )
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an operator with hierarchy and comparisons" $ do
      let input = "n is 0 or n is 1"
      let env = PS.insertVar "n" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Op
                PU.normalizeLoc
                AT.Or
                ( AT.Op
                    PU.normalizeLoc
                    AT.Eq
                    (AT.Var PU.normalizeLoc "n" $ AT.TInt 32)
                    (AT.Lit PU.normalizeLoc $ AT.LInt 0)
                )
                ( AT.Op
                    PU.normalizeLoc
                    AT.Eq
                    (AT.Var PU.normalizeLoc "n" $ AT.TInt 32)
                    (AT.Lit PU.normalizeLoc $ AT.LInt 1)
                )
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a unary operator" $ do
      let input = "not 1"
      result <- parse input
      let expected =
            Right $
              AT.UnaryOp
                PU.normalizeLoc
                AT.Not
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)

      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a line comment" $ do
      let input = "not 1 % this is a line comment"
      result <- parse input
      let expected =
            Right $
              AT.UnaryOp
                PU.normalizeLoc
                AT.Not
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a block comment" $ do
      let input = "not 1 %% this is a block comment %%"
      result <- parse input
      let expected =
            Right $
              AT.UnaryOp
                PU.normalizeLoc
                AT.Not
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a higher order function" $ do
      let input = "map: (int -> char) int -> char = f x { f(x) }"
      let functionType = AT.TFunction AT.TChar [AT.TInt 32] False
      result <- parse input
      let expected =
            Right $
              AT.Function
                PU.normalizeLoc
                "map"
                (AT.TFunction AT.TChar [functionType, AT.TInt 32] False)
                ["f", "x"]
                ( AT.Block
                    [ AT.Return PU.normalizeLoc $ Just $ AT.Call PU.normalizeLoc (AT.Var PU.normalizeLoc "f" functionType) [AT.Var PU.normalizeLoc "x" $ AT.TInt 32]
                    ]
                )
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a prefix operator" $ do
      let input = "++x"
      let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.UnaryOp PU.normalizeLoc AT.PreInc (AT.Var PU.normalizeLoc "x" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a postfix operator" $ do
      let input = "x++"
      let env = PS.insertVar "x" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.UnaryOp PU.normalizeLoc AT.PostInc (AT.Var PU.normalizeLoc "x" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a deref operator" $ do
      let input = "x.*"
      let varType = AT.TPointer $ AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.UnaryOp PU.normalizeLoc AT.Deref (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a deref assignment" $ do
      let input = "x.* = 1"
      let varType = AT.TPointer $ AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected =
            Right $
              AT.Assignment
                PU.normalizeLoc
                (AT.UnaryOp PU.normalizeLoc AT.Deref (AT.Var PU.normalizeLoc "x" varType))
                (AT.Lit PU.normalizeLoc $ AT.LInt 1)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a foreign function" $ do
      let input = "print: foreign int -> never"
      result <- parse input
      let expected =
            Right $
              AT.ForeignFunction
                PU.normalizeLoc
                "print"
                (AT.TFunction AT.TVoid [AT.TInt 32] False)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a foreign function with a function declaration after it" $
      do
        let input = "free: foreign *byte -> never\nmain: never -> int = { free(0) 0 }"
        result <- parseMany input
        let expected =
              Right $
                AT.Block
                  [ AT.ForeignFunction
                      PU.normalizeLoc
                      "free"
                      (AT.TFunction AT.TVoid [AT.TPointer (AT.TInt 8)] False),
                    AT.Function
                      PU.normalizeLoc
                      "main"
                      (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
                      []
                      ( AT.Block
                          [ AT.Call
                              PU.normalizeLoc
                              (AT.Var PU.normalizeLoc "free" (AT.TFunction AT.TVoid [AT.TPointer (AT.TInt 8)] False))
                              [AT.Lit PU.normalizeLoc (AT.LInt 0)],
                            AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 0)))
                          ]
                      )
                  ]
        (PU.normalizeExpr . AT.Block <$> result) `shouldBe` expected

    -- it "parses a function with only a defer inside it" $
    --   do
    --     let input = "main: never -> int = { defer 1 }"
    --     let result = PU.normalizeExpr <$> parseWithEnv input
    --     let expected =
    --           Right $
    --             AT.Function
    --               PU.normalizeLoc
    --               "main"
    --               (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
    --               []
    --               ( AT.Block
    --                   [ AT.Lit PU.normalizeLoc (AT.LInt 1)
    --                   ]
    --               )
    --     result `shouldBe` expected

    it "parses a function with a defer and a return statement" $
      do
        let input = "main: never -> int = { defer 1 ret 42 }"
        result <- parse input
        let expected =
              Right $
                AT.Function
                  PU.normalizeLoc
                  "main"
                  (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
                  []
                  ( AT.Block
                      [ AT.Lit PU.normalizeLoc (AT.LInt 1),
                        AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 42)))
                      ]
                  )
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a function with a defer, an if statement, and a return" $
      do
        let input = "main: never -> int = { defer free(0) if 1 is 0 { ret 2 } ret 42 }"
        let varType = AT.TFunction AT.TVoid [AT.TPointer $ AT.TInt 32] False
        let env = PS.insertVar "free" varType PS.parserState
        result <- parseWithCustom env input
        let expected =
              Right $
                AT.Function
                  PU.normalizeLoc
                  "main"
                  (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
                  []
                  ( AT.Block
                      [ AT.If
                          PU.normalizeLoc
                          ( AT.Op
                              PU.normalizeLoc
                              AT.Eq
                              (AT.Lit PU.normalizeLoc (AT.LInt 1))
                              (AT.Lit PU.normalizeLoc (AT.LInt 0))
                          )
                          (AT.Block [AT.Return PU.normalizeLoc (Just (AT.Lit PU.normalizeLoc (AT.LInt 2)))])
                          Nothing,
                        AT.Call
                          PU.normalizeLoc
                          (AT.Var PU.normalizeLoc "free" varType)
                          [AT.Lit PU.normalizeLoc $ AT.LInt 0],
                        AT.Return
                          PU.normalizeLoc
                          (Just (AT.Lit PU.normalizeLoc (AT.LInt 42)))
                      ]
                  )
        (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses inline assembly" $ do
      let input = "__asm__ { code -> \"nop\" constraints -> \"\" args -> () parameters -> never return_type -> never side_effects -> false align_stack -> false dialect -> ATT }"
      result <- parse input
      let expected =
            Right $
              AT.Assembly
                PU.normalizeLoc
                (AT.AsmExpr "nop" (AT.AsmConstraint "" []) [] [AT.TVoid] AT.TVoid False False AT.ATT)
      (PU.normalizeExpr <$> result) `shouldBe` expected
