## Compiler Structure

The Frost compiler is built on a modular architecture that follows the
traditional compiler pipeline while incorporating modern design principles. This
structure allows for clear separation of concerns, extensibility, and
maintainability.

### Frontend

The frontend of the Frost compiler is primarily handled by the modules in the
`lib/Ast/` directory. It consists of two main stages: lexical analysis and
parsing.

#### Lexical Analysis and Parsing

The lexer and parser are implemented in the `lib/Ast/Parser/` directory. The
parsing process transforms the source code into an Abstract Syntax Tree (AST)
through a series of parser combinators defined in files such as `Expr.hs`,
`Literal.hs`, and `Program.hs`.

The `Parser.hs` file serves as the entry point for the parsing process,
coordinating the various parsing components. It handles the creation of the AST,
which is defined in `Ast/Types.hs`. The AST representation includes all
necessary information about the program's structure, as shown in this simplified
example:

```haskell
data Program = Program {
    globals :: [(String, Expr)],
    types :: [(String, Type)],
    sourceFile :: String
}
```

#### Preprocessor

The Frost compiler includes a preprocessor, implemented in
`lib/Ast/Parser/PreProcessor/`. This component handles directives such as `?set`
and `?defined`, allowing for macro expansions and file inclusions before the
main parsing stage.

### Middle-end

The middle-end of the compiler focuses on semantic analysis and intermediate
representations.

#### Semantic Analysis

Type checking and inference are crucial parts of the Frost compiler's
middle-end. The type system, defined in `lib/Ast/Types.hs`, ensures that all
expressions and statements in the program are type-safe. This includes
validating user-defined types and handling type inference where applicable.

### Backend

The backend of the Frost compiler is primarily concerned with code generation
and is implemented in the `lib/Codegen/` directory.

#### Code Generation

The code generation process is orchestrated by `Codegen.hs`, which transforms
the AST into LLVM IR. This process is broken down into several submodules in the
`ExprGen/` directory, each responsible for generating code for different
language constructs.

The code generation process utilizes a state monad to manage the compilation
context, as defined in `Codegen/State.hs`:

```haskell
data CodegenState = CodegenState {
    localState :: LocalState,
    globalState :: GlobalState,
    loopState :: LoopState,
    allocatedVars :: LocalState,
    uniqueNameState :: UniqueNameState
}
```

### Error Handling

Error handling is a critical aspect of the Frost compiler. The
`Codegen/Errors.hs` module defines various error types and provides mechanisms
for error reporting. This includes syntax errors caught during parsing, type
errors identified during semantic analysis, and code generation failures.

### Testing

The compiler includes a comprehensive test suite in the `test/` directory. This
includes unit tests for parser components (`test/Ast/Parser/`) and integration
tests for code generation (`test/Codegen/`).

### Standard Library

Frost comes with a standard library implemented in the `examples/std/`
directory. This includes modules for I/O operations, mathematical functions,
networking, and more.

This architecture allows the Frost compiler to be both powerful and flexible,
capable of handling complex language features while remaining maintainable and
extensible. The clear separation of concerns and modular design facilitate
future enhancements and optimizations.
