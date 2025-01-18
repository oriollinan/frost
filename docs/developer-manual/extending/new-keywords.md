## Adding New Keywords to Frost

To extend the Frost language with new keywords, you'll need to modify several
components of the compiler. Here's a step-by-step guide using the provided code
as a reference:

### 1. Update the Parser

In the `Ast.Parser.Expr` module, add a new parsing function for your keyword:

```haskell
parseNewKeyword :: PU.Parser AT.Expr
parseNewKeyword = do
  srcLoc <- PU.parseSrcLoc
  _ <- PU.symbol "newkeyword"
  -- Add parsing logic for the new keyword
  -- For example:
  expr <- parseExpr
  return $ AT.NewKeyword srcLoc expr
```

Then, add this new parser to the `parseTerm` function:

```haskell
parseTerm :: PU.Parser AT.Expr
parseTerm =
  M.choice
    [ -- ... existing choices ...
    , parseNewKeyword
    , parseParenExpr
    ]
```

### 2. Extend the AST

In the `Ast.Types` module, add a new constructor to the `Expr` data type:

```haskell
data Expr
  = -- ... existing constructors ...
  | NewKeyword SrcLoc Expr
```

### 3. Implement Semantic Analysis

If your new keyword requires semantic analysis, update the relevant functions in
the semantic analysis phase. This might involve modifying the type checker or
other analysis passes.

### 4. Add Code Generation

Implement code generation for your new keyword in the appropriate module within
`Codegen.ExprGen`:

```haskell
-- | Generate LLVM IR for a new keyword expression.
generateNewKeyword :: AT.Expr -> AT.Expr -> Codegen AST.Operand
generateNewKeyword loc expr = do
  -- Generate LLVM IR for your new keyword
  -- For example:
  exprOp <- generateExpr expr
  -- Add LLVM instructions as needed
  return exprOp
```

### 5. Update Error Handling

In `Codegen.Errors`, add any new error types related to your keyword:

```haskell
data CodegenErrorType
  = -- ... existing error types ...
  | InvalidNewKeywordUsage AT.Expr
```

### 6. Modify Utility Functions

Update utility functions like `getLoc` in `Shared.Utils` to handle your new
expression type:

```haskell
getLoc :: AT.Expr -> AT.SrcLoc
getLoc expr = case expr of
  -- ... existing cases ...
  AT.NewKeyword loc _ -> loc
```

### 7. Add Tests

Create new test cases in the `test/` directory to ensure your keyword works as
expected:

```haskell
testNewKeyword :: Test
testNewKeyword = TestCase $ do
  let code = "newkeyword someExpression"
  result <- runParser parseExpr code
  assertEqual "New keyword parsing" expectedAST result
```

### 8. Update Documentation

Don't forget to update the language documentation to include information about
your new keyword, its syntax, and usage examples.

By following these steps and integrating your changes with the existing
codebase, you can successfully extend Frost with new keywords while maintaining
its integrity and consistency.
