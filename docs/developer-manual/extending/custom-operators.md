## Creating Custom Operators in Frost

Frost allows developers to extend the language with custom operators, providing
flexibility and expressiveness for domain-specific needs. This guide will walk
you through the process of implementing new operators in the Frost compiler.

### Overview

Custom operators in Frost are implemented by modifying the parser and adding new
entries to the `operationTable`. This table defines the precedence and
associativity of operators.

### Steps to Add a Custom Operator

1. **Define the Operator in AST Types**

First, add your new operator to the `Operation` data type in `Ast/Types.hs`:

```haskell
data Operation
  = -- ... existing operators ...
  | YourNewOperator
  deriving (Show, Eq, Ord)
```

2. **Update the Parser**

In `Ast/Parser/Expr.hs`, add your new operator to the `operationTable`:

```haskell
operationTable :: [[CE.Operator PU.Parser AT.Expr]]
operationTable =
  [ -- ... existing operator groups ...
    [ PU.binary "your_operator_symbol" (`AT.Op` AT.YourNewOperator)
    ]
    -- ... other operator groups ...
  ]
```

3. **Implement Code Generation**

Update the code generation logic in `Codegen/ExprGen/Operator.hs` to handle your
new operator:

```haskell
generateBinaryOp :: AT.Operation -> AST.Operand -> AST.Operand -> Codegen AST.Operand
generateBinaryOp op left right = case op of
  -- ... existing cases ...
  AT.YourNewOperator -> -- Implement your operator logic here
```

4. **Update Error Handling**

Ensure that `Codegen/Errors.hs` can handle potential errors related to your new
operator:

```haskell
data CodegenErrorType
  = -- ... existing error types ...
  | InvalidYourNewOperatorUsage
  deriving (Show)
```

5. **Add Tests**

Create unit tests for your new operator in the `test/` directory:

```haskell
testYourNewOperator :: Test
testYourNewOperator = TestCase $ do
  let code = "a your_operator_symbol b"
  result <- runParser parseExpr code
  assertEqual "Your new operator test" expectedAST result
```

### Best Practices

- Ensure your operator has a clear and intuitive meaning
- Document the operator's behavior thoroughly
- Consider precedence carefully when adding to the `operationTable`
- Implement comprehensive error checking and reporting

### Example: Adding a Power Operator

Let's add a power operator `**` as an example:

1. In `Ast/Types.hs`:

```haskell
data Operation
  = -- ... existing operators ...
  | Power
  deriving (Show, Eq, Ord)
```

2. In `Ast/Parser/Expr.hs`:

```haskell
operationTable =
  [ -- ...
    [ PU.binary "**" (`AT.Op` AT.Power)
    ]
    -- ...
  ]
```

3. In `Codegen/ExprGen/Operator.hs`:

<!-- deno-fmt-ignore -->
{% hint style='info' %}
This implementation uses the `llvm.pow.f64` function to calculate the power of two floating-point numbers. If you want to implement other types, you will need to extend this function accordingly.
{% endhint %}

```haskell
generateBinaryOp AT.Power left right = do
  powFunc <- externf (AST.FloatingPointType 64) "llvm.pow.f64" [(AST.FloatingPointType 64, []), (AST.FloatingPointType 64, [])]
  call powFunc [(left, []), (right, [])]
```

By following these steps and best practices, you can extend Frost with powerful
custom operators that enhance its capabilities for specific domains or
programming paradigms.
