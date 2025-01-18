## Implementing New Types in Frost

Extending Frost with new types involves modifying the parser, type system, and
code generation components. This guide will walk you through the process of
adding a new type to the language.

### 1. Update the AST

First, add the new type to the `Type` data type in `lib/Ast/Types.hs`:

```haskell
data Type
  = -- ... existing types ...
  | TNewType -- Add your new type here
  deriving (Show, Eq, Ord)
```

### 2. Modify the Parser

Update the type parser in `lib/Ast/Parser/Type.hs` to recognize the new type:

```haskell
parseType :: PU.Parser AT.Type
parseType = M.choice
  [ -- ... existing choices ...
  , parseNewType
  ]

parseNewType :: PU.Parser AT.Type
parseNewType = AT.TNewType <$ PU.symbol "newtype"
```

### 3. Add Code Generation

Implement code generation for your new type in the appropriate file within
`lib/Codegen/`:

```haskell
generateType :: AT.Type -> Codegen AST.Type
generateType ty = case ty of
  -- ... existing cases ...
  AT.TNewType -> do
    -- Generate LLVM type for your new type
    return $ AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0)
```

### 4. Update Utility Functions

Modify utility functions that work with types to handle the new type. For
example, in `lib/Shared/Utils.hs`:

```haskell
sizeOfType :: AT.Type -> Int
sizeOfType ty = case ty of
  -- ... existing cases ...
  AT.TNewType -> 8 -- Specify the size of your new type
```

### 5. Add Tests

Create new test cases to ensure your type works as expected. Add tests in the
`test/` directory:

```haskell
testNewType :: Test
testNewType = TestCase $ do
  let code = "x: newtype = ..."
  result <- runTypeChecker code
  assertEqual "New type test" expectedType result
```

### 5. Update Documentation

Update the language documentation to include information about your new type,
its syntax, and usage examples.

### Best Practices

- Ensure your new type integrates well with existing language features.
- Consider the impact on type inference and type compatibility.
- Provide clear error messages for type-related issues.
- Follow Frost's naming conventions and code style.

By following these steps, you can successfully extend Frost with new types,
enhancing the language's expressiveness and capabilities while maintaining its
integrity and consistency.
