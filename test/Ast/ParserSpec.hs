module Ast.ParserSpec (spec) where

import Ast.Parser (parse)
import Ast.Types
  ( AST (..),
    Expr (..),
    Literal (..),
    Operation (..),
  )
import Test.Hspec

spec :: Spec
spec = do
  describe "Literal Parsing" $ do
    -- \**Integer Literal Parsing Tests **
    it "parses an integer literal" $ do
      parse "" "42" `shouldBe` Right (AST [Lit (LInt 42)])

    it "parses a negative integer literal" $ do
      parse "" "-7" `shouldBe` Right (AST [Lit (LInt (-7))])

    it "fails to parse a malformed integer literal" $ do
      parse "" "42abc" `shouldSatisfy` isLeft

    -- \**Bolean Literal Parsing Tests **
    it "parses a boolean literal '#t'" $ do
      parse "" "#t" `shouldBe` Right (AST [Lit (LBool True)])

    it "parses a boolean literal '#f'" $ do
      parse "" "#f" `shouldBe` Right (AST [Lit (LBool False)])

    it "fails to parse an invalid boolean literal" $ do
      parse "" "#invalid" `shouldSatisfy` isLeft

    it "parses a symbol literal" $ do
      parse "" "(define foo_bar 10) foo_bar" `shouldBe` Right (AST [Define "foo_bar" (Lit (LInt 10)), Var "foo_bar"])

  -- \**Separator Parsing Tests **
  describe "List Parsing" $ do
    it "parses an empty list" $ do
      parse "" "()" `shouldBe` Right (AST [Seq []])

  -- \**Define Expression Tests **
  describe "Define Expressions" $ do
    it "parses a simple define with a variable" $ do
      parse "" "(define x 10)"
        `shouldBe` Right
          ( AST
              [Define "x" (Lit (LInt 10))]
          )

    it "parses a define with a function" $ do
      parse "" "(define (add a b) (+ a b))"
        `shouldBe` Right
          ( AST
              [ Define
                  "add"
                  ( Lambda
                      ["a", "b"]
                      (Op Add (Var "a") (Var "b"))
                  )
              ]
          )

    it "fails to parse a define with invalid arguments" $ do
      parse "" "(define 42 10)" `shouldSatisfy` isLeft

  -- \**If Expression Tests **
  describe "If Expressions" $ do
    it "parses a simple if expression" $ do
      parse "" "(if #t 1 0)"
        `shouldBe` Right
          ( AST
              [If (Lit (LBool True)) (Lit (LInt 1)) (Lit (LInt 0))]
          )

    it "parses a nested if expression" $ do
      parse "" "(if (== 1 1) #t #f)"
        `shouldBe` Right
          ( AST
              [ If
                  (Op Equal (Lit (LInt 1)) (Lit (LInt 1)))
                  (Lit (LBool True))
                  (Lit (LBool False))
              ]
          )

  -- \**Lambda Expression Tests **
  describe "Lambda Expressions" $ do
    it "parses a simple lambda expression" $ do
      parse "" "(lambda (x) (* x x))"
        `shouldBe` Right
          ( AST
              [ Lambda
                  ["x"]
                  (Op Mult (Var "x") (Var "x"))
              ]
          )

    it "parses a lambda with multiple parameters" $ do
      parse "" "(lambda (x y) (+ x y))"
        `shouldBe` Right
          ( AST
              [ Lambda
                  ["x", "y"]
                  (Op Add (Var "x") (Var "y"))
              ]
          )

    it "fails to parse a lambda with invalid parameter list" $ do
      parse "" "(lambda 42 (* 2 2))" `shouldSatisfy` isLeft

  -- \**Function Call Tests **
  describe "Function Calls" $ do
    it "parses a simple function call" $ do
      parse "" "(define (add x y) (+ x y)) (add 1 2)"
        `shouldBe` Right
          ( AST
              [ Define
                  "add"
                  ( Lambda
                      ["x", "y"]
                      (Op Add (Var "x") (Var "y"))
                  ),
                Call
                  (Var "add")
                  (Seq [Lit (LInt 1), Lit (LInt 2)])
              ]
          )

    it "parses a nested function call" $ do
      parse "" "(* (+ 1 2) 3)"
        `shouldBe` Right
          ( AST
              [ Op
                  Mult
                  (Op Add (Lit (LInt 1)) (Lit (LInt 2)))
                  (Lit (LInt 3))
              ]
          )

  -- \**Operation Expression Tests **
  describe "Operation Expressions" $ do
    it "parses an addition operation" $ do
      parse "" "(+ 1 2)"
        `shouldBe` Right
          ( AST
              [ Op Add (Lit (LInt 1)) (Lit (LInt 2))
              ]
          )

    it "parses a subtraction operation" $ do
      parse "" "(- 5 3)"
        `shouldBe` Right
          ( AST
              [ Op Sub (Lit (LInt 5)) (Lit (LInt 3))
              ]
          )

    it "parses a multiplication operation" $ do
      parse "" "(* 4 2)"
        `shouldBe` Right
          ( AST
              [ Op Mult (Lit (LInt 4)) (Lit (LInt 2))
              ]
          )

    it "parses a division operation" $ do
      parse "" "(div 10 2)"
        `shouldBe` Right
          ( AST
              [ Op Div (Lit (LInt 10)) (Lit (LInt 2))
              ]
          )

    it "parses a modulo operation" $ do
      parse "" "(mod 10 2)"
        `shouldBe` Right
          ( AST
              [ Op Mod (Lit (LInt 10)) (Lit (LInt 2))
              ]
          )

    it "parses a greater than operation" $ do
      parse "" "(define a 10) (define b 5) (> a b)"
        `shouldBe` Right
          ( AST
              [ Define "a" (Lit (LInt 10)),
                Define "b" (Lit (LInt 5)),
                Op Gt (Var "a") (Var "b")
              ]
          )

    it "parses a not equal operation" $ do
      parse "" "(define a 10) (define b 5) (/= a b)"
        `shouldBe` Right
          ( AST
              [ Define "a" (Lit (LInt 10)),
                Define "b" (Lit (LInt 5)),
                Op Ne (Var "a") (Var "b")
              ]
          )

    it "parses a logical AND operation" $ do
      parse "" "(&& #t #f)"
        `shouldBe` Right (AST [Op And (Lit (LBool True)) (Lit (LBool False))])

    it "parses a complex expression with mixed operations" $ do
      parse "" "(+ (* 2 3) (div 10 2))"
        `shouldBe` Right (AST [Op Add (Op Mult (Lit (LInt 2)) (Lit (LInt 3))) (Op Div (Lit (LInt 10)) (Lit (LInt 2)))])

  -- \**Multiple Expressions Tests **
  describe "Multiple Expressions" $ do
    it "parses multiple expressions in sequence" $ do
      parse "" "(define x 10) (define y 20)"
        `shouldBe` Right
          ( AST
              [ Define "x" (Lit (LInt 10)),
                Define "y" (Lit (LInt 20))
              ]
          )

    it "parses a define followed by a function call" $ do
      parse "" "(define x 5) (+ x 3)"
        `shouldBe` Right
          ( AST
              [ Define "x" (Lit (LInt 5)),
                Op Add (Var "x") (Lit (LInt 3))
              ]
          )

  -- \**Error Handling Tests **
  describe "Error Handling" $ do
    it "fails to parse an incomplete expression" $ do
      parse "" "(define x" `shouldSatisfy` isLeft

    it "fails to parse malformed if expression" $ do
      parse "" "(if #t 1)" `shouldSatisfy` isLeft

    it "fails to parse an undefined variable" $ do
      parse "" "x" `shouldSatisfy` isLeft

    it "fails to parse a call to an undefined lambda" $ do
      parse "" "(undefined-lambda 1)" `shouldSatisfy` isLeft

    it "fails to parse a reserved keyword as a variable name" $ do
      parse "" "(define lambda 10)" `shouldSatisfy` isLeft

    it "parses deeply nested lists" $ do
      parse "" "((((1))))" `shouldBe` Right (AST [Seq [Seq [Seq [Seq [Lit (LInt 1)]]]]])

-- | Helper function to check if a result is a Left (error)
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
