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

    it "parses a dereference operator" $ do
      let input = "x.*"
      let varType = AT.TPointer (AT.TInt 32)
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.Deref (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an address-of operator" $ do
      let input = "x.&"
      let varType = AT.TPointer (AT.TInt 32)
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.AddrOf (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a post-increment operator" $ do
      let input = "x++"
      let varType = AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.PostInc (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a post-decrement operator" $ do
      let input = "x--"
      let varType = AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.PostDec (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a logical NOT operator" $ do
      let input = "!x"
      let varType = AT.TBoolean
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.Not (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a bitwise NOT operator" $ do
      let input = "~x"
      let varType = AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.BitNot (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a pre-increment operator" $ do
      let input = "++x"
      let varType = AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.PreInc (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a pre-decrement operator" $ do
      let input = "--x"
      let varType = AT.TInt 32
      let env = PS.insertVar "x" varType PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.UnaryOp PU.normalizeLoc AT.PreDec (AT.Var PU.normalizeLoc "x" varType)
      (PU.normalizeExpr <$> result) `shouldBe` expected
    it "parses an addition operator" $ do
      let input = "x + y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Add (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a bitwise AND operator" $ do
      let input = "x & y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.BitAnd (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a bitwise OR operator" $ do
      let input = "x | y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.BitOr (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a bitwise XOR operator" $ do
      let input = "x ^ y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.BitXor (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a bitwise left shift operator" $ do
      let input = "x << y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.BitShl (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a bitwise right shift operator" $ do
      let input = "x >> y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.BitShr (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a less than or equal comparison" $ do
      let input = "x <= y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Lte (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses an equality operator (==)" $ do
      let input = "x == y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Eq (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a not equal operator (!=)" $ do
      let input = "x != y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Ne (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a greater than or equal operator (>=)" $ do
      let input = "x >= y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Gte (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a less than operator (<)" $ do
      let input = "x < y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Lt (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a greater than operator (>)" $ do
      let input = "x > y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Gt (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected
    it "parses a division operator (/)" $ do
      let input = "x / y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Div (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a modulus operator (mod)" $ do
      let input = "x mod y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Mod (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a subtraction operator (-)" $ do
      let input = "x - y"
      let env = PS.insertVar "x" (AT.TInt 32) $ PS.insertVar "y" (AT.TInt 32) PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.Sub (AT.Var PU.normalizeLoc "x" $ AT.TInt 32) (AT.Var PU.normalizeLoc "y" $ AT.TInt 32)
      (PU.normalizeExpr <$> result) `shouldBe` expected

    it "parses a logical AND operator (and)" $ do
      let input = "x and y"
      let env = PS.insertVar "x" AT.TBoolean $ PS.insertVar "y" AT.TBoolean PS.parserState
      result <- parseWithCustom env input
      let expected = Right $ AT.Op PU.normalizeLoc AT.And (AT.Var PU.normalizeLoc "x" AT.TBoolean) (AT.Var PU.normalizeLoc "y" AT.TBoolean)
      (PU.normalizeExpr <$> result) `shouldBe` expected

  describe "implicitReturn" $ do
    it "wraps a literal in a return" $ do
      let input = AT.Lit PU.normalizeLoc (AT.LInt 123)
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a variable in a return" $ do
      let input = AT.Var PU.normalizeLoc "x" (AT.TInt 32)
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a function in a return" $ do
      let input =
            AT.Function
              PU.normalizeLoc
              "main"
              (AT.TFunction (AT.TInt 32) [AT.TVoid] False)
              []
              (AT.Block [])
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a foreign function in a return" $ do
      let input =
            AT.ForeignFunction
              PU.normalizeLoc
              "print"
              (AT.TFunction AT.TVoid [AT.TInt 32] False)
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a declaration in a return" $ do
      let input =
            AT.Declaration
              PU.normalizeLoc
              "x"
              (AT.TInt 32)
              (Just $ AT.Lit PU.normalizeLoc (AT.LInt 42))
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps an assignment in a return" $ do
      let input =
            AT.Assignment
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "x" (AT.TInt 32))
              (AT.Lit PU.normalizeLoc (AT.LInt 42))
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a function call in a return" $ do
      let input =
            AT.Call
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "foo" (AT.TFunction (AT.TInt 32) [AT.TInt 32] False))
              [AT.Lit PU.normalizeLoc (AT.LInt 123)]
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a binary operation in a return" $ do
      let input =
            AT.Op
              PU.normalizeLoc
              AT.Add
              (AT.Lit PU.normalizeLoc (AT.LInt 1))
              (AT.Lit PU.normalizeLoc (AT.LInt 2))
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a unary operation in a return" $ do
      let input =
            AT.UnaryOp
              PU.normalizeLoc
              AT.Not
              (AT.Lit PU.normalizeLoc (AT.LInt 1))
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a struct access in a return" $ do
      let input =
            AT.StructAccess
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "myStruct" (AT.TStruct "Custom" []))
              (AT.Var PU.normalizeLoc "myField" AT.TUnknown)
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps an array access in a return" $ do
      let input =
            AT.ArrayAccess
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "myArray" (AT.TArray (AT.TInt 32) Nothing))
              (AT.Lit PU.normalizeLoc (AT.LInt 1))
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps a type cast in a return" $ do
      let input =
            AT.Cast
              PU.normalizeLoc
              (AT.TInt 32)
              (AT.Lit PU.normalizeLoc (AT.LChar 'a'))
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "wraps an assembly expression in a return" $ do
      let input =
            AT.Assembly
              PU.normalizeLoc
              (AT.AsmExpr "nop" (AT.AsmConstraint "" []) [] [AT.TVoid] AT.TVoid False False AT.ATT)
      let expected = AT.Return PU.normalizeLoc (Just input)
      PE.implicitReturn input `shouldBe` expected

    it "does not wrap a while loop" $ do
      let input =
            AT.While
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "x" (AT.TInt 32))
              (AT.Block [])
      PE.implicitReturn input `shouldBe` input

    it "does not wrap a for loop" $ do
      let input =
            AT.For
              PU.normalizeLoc
              (AT.Declaration PU.normalizeLoc "i" (AT.TInt 32) (Just $ AT.Lit PU.normalizeLoc (AT.LInt 0)))
              (AT.Op PU.normalizeLoc AT.Lt (AT.Var PU.normalizeLoc "i" (AT.TInt 32)) (AT.Lit PU.normalizeLoc (AT.LInt 10)))
              (AT.Assignment PU.normalizeLoc (AT.Var PU.normalizeLoc "i" (AT.TInt 32)) (AT.Op PU.normalizeLoc AT.Add (AT.Var PU.normalizeLoc "i" (AT.TInt 32)) (AT.Lit PU.normalizeLoc (AT.LInt 1))))
              (AT.Block [])
      PE.implicitReturn input `shouldBe` input

    it "does not wrap a return statement" $ do
      let input = AT.Return PU.normalizeLoc (Just $ AT.Lit PU.normalizeLoc (AT.LInt 123))
      PE.implicitReturn input `shouldBe` input

    it "does not wrap a break statement" $ do
      let input = AT.Break PU.normalizeLoc
      PE.implicitReturn input `shouldBe` input

    it "does not wrap a continue statement" $ do
      let input = AT.Continue PU.normalizeLoc
      PE.implicitReturn input `shouldBe` input

    it "applies implicit return to the last expression in a block" $ do
      let input =
            AT.Block
              [ AT.Assignment PU.normalizeLoc (AT.Var PU.normalizeLoc "x" (AT.TInt 32)) (AT.Lit PU.normalizeLoc (AT.LInt 42)),
                AT.Lit PU.normalizeLoc (AT.LInt 123)
              ]
      let expected =
            AT.Block
              [ AT.Assignment PU.normalizeLoc (AT.Var PU.normalizeLoc "x" (AT.TInt 32)) (AT.Lit PU.normalizeLoc (AT.LInt 42)),
                AT.Return PU.normalizeLoc (Just $ AT.Lit PU.normalizeLoc (AT.LInt 123))
              ]
      PE.implicitReturn input `shouldBe` expected

    it "applies implicit return to the then branch of an if expression without an else branch" $ do
      let input =
            AT.If
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "cond" AT.TBoolean)
              (AT.Block [AT.Lit PU.normalizeLoc (AT.LInt 123)])
              Nothing
      let expected =
            AT.If
              PU.normalizeLoc
              (AT.Var PU.normalizeLoc "cond" AT.TBoolean)
              (AT.Block [AT.Return PU.normalizeLoc (Just $ AT.Lit PU.normalizeLoc (AT.LInt 123))])
              Nothing
      PE.implicitReturn input `shouldBe` expected

    it "returns an empty block unchanged" $ do
      let input = AT.Block []
      let expected = AT.Block []
      PE.implicitReturn input `shouldBe` expected

  describe "ParserState import-related functions" $ do
    it "looks up an existing import in the parser state" $ do
      let inputImport = "MyModule"
      let stateWithImport = PS.insertImport inputImport PS.parserState
      PS.lookupImport inputImport stateWithImport `shouldBe` True

    it "does not find a non-existent import in the parser state" $ do
      let inputImport = "NonExistentModule"
      PS.lookupImport inputImport PS.parserState `shouldBe` False

    it "sets the import depth in the parser state" $ do
      let newDepth = 5
      let updatedState = PS.setImportDepth newDepth PS.parserState
      PS.recursionDepth (PS.importState updatedState) `shouldBe` newDepth

    it "retrieves the import depth from the parser state" $ do
      let initialDepth = 3
      let stateWithDepth = PS.setImportDepth initialDepth PS.parserState
      PS.getImportDepth stateWithDepth `shouldBe` initialDepth
