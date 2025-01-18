## Building the Abstract Syntax Tree (AST)

The Abstract Syntax Tree (AST) is a crucial intermediate representation in the
Frost compiler. It captures the structure and semantics of the source code,
serving as the foundation for subsequent compilation stages. This document
outlines the process of building the AST in Frost.

### Parser Structure

The AST construction begins in the `Ast.Parser` module, which orchestrates the
parsing process:

```haskell
parse :: String -> String -> IO (Either String AT.Program)
parse sourceFile input = do
  (result, _) <- S.runStateT (M.runParserT (PP.parseProgram sourceFile) sourceFile input) PS.parserState
  case result of
    (Left err) -> return $ Left (M.errorBundlePretty err)
    (Right program) -> return $ Right program
```

This function takes the source file name and input string, runs the parser, and
returns either an error message or the parsed AST.

### AST Node Types

The AST is composed of various node types defined in `Ast.Types`. Key components
include:

1. **Source Location**: For error reporting and debugging.
   ```haskell
   data SrcLoc = SrcLoc
     { srcFile :: String,
       srcLine :: Int,
       srcCol :: Int
     }
   ```

2. **Literals**: Representing constant values.
   ```haskell
   data Literal
     = LInt Integer
     | LFloat Double
     | LDouble Double
     | LChar Char
     | LBool Bool
     | LArray [Literal]
     | LNull
     | LStruct [(String, Literal)]
   ```

3. **Types**: Representing Frost's type system.
   ```haskell
   data Type
     = TInt Int
     | TFloat
     | TDouble
     | TChar
     | TBoolean
     | TVoid
     | TMutable Type
     | TPointer Type
     | TArray Type (Maybe Int)
     | TFunction { returnType :: Type, paramTypes :: [Type], isVariadic :: Bool }
     | TStruct { structName :: String, fields :: [(String, Type)] }
     | TUnion { unionName :: String, variants :: [(String, Type)] }
     | TTypedef String Type
     | TUnknown
   ```

4. **Expressions**: Representing various language constructs.
   ```haskell
   data Expr
     = Lit SrcLoc Literal
     | Var SrcLoc String Type
     | Function { funcLoc :: SrcLoc, funcName :: String, funcType :: Type, funcParams :: [String], funcBody :: Expr }
     | ForeignFunction { funcLoc :: SrcLoc, funcName :: String, funcType :: Type }
     | Declaration { declLoc :: SrcLoc, declName :: String, declType :: Type, declInit :: Maybe Expr }
     | Assignment { assignLoc :: SrcLoc, assignTarget :: Expr, assignValue :: Expr }
     | Call { callLoc :: SrcLoc, callFunc :: Expr, callArgs :: [Expr] }
     | If { ifLoc :: SrcLoc, ifCond :: Expr, ifThen :: Expr, ifElse :: Maybe Expr }
     | While { whileLoc :: SrcLoc, whileCond :: Expr, whileBody :: Expr }
     | From { fromLoc :: SrcLoc, fromStart :: Expr, fromEnd :: Expr, fromStep :: Expr, fromVar :: Expr, fromBody :: Expr }
     | Block [Expr]
     | Return SrcLoc (Maybe Expr)
     | Break SrcLoc
     | Continue SrcLoc
     | Op SrcLoc Operation Expr Expr
     | UnaryOp SrcLoc UnaryOperation Expr
     | StructAccess SrcLoc Expr Expr
     | ArrayAccess SrcLoc Expr Expr
     | Cast SrcLoc Type Expr
     | Assembly { asmLoc :: SrcLoc, asmExpr :: AsmExpr }
   ```

### Parsing Process

1. **Tokenization**: The input string is broken down into tokens.
2. **Recursive Descent**: The parser uses recursive descent to build the AST
   bottom-up.
3. **Node Construction**: As language constructs are recognized, corresponding
   AST nodes are created.
4. **Type Annotation**: Types are inferred or explicitly annotated during
   parsing.
5. **Error Handling**: Syntax errors are caught and reported with precise source
   locations.

### AST Traversal and Manipulation

After construction, the AST can be traversed and manipulated. The `Shared.Utils`
module provides utility functions for this purpose:

```haskell
getLoc :: AT.Expr -> AT.SrcLoc
getLoc expr = case expr of
  AT.Lit loc _ -> loc
  AT.Var loc _ _ -> loc
  -- ... (other cases)
```

This function extracts the source location from an AST node, which is crucial
for error reporting and code generation.

Building the AST is a critical step in the Frost compilation process. It
transforms the flat text of the source code into a structured representation
that captures the semantics of the program. This representation serves as the
foundation for subsequent stages such as type checking, optimization, and code
generation.
