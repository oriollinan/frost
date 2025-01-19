## Project Overview

The Frost compiler, written in Haskell, is organized into a modular architecture
that emphasizes clean separation of concerns and maintainable code. The project
structure follows a logical organization that supports the complete compilation
pipeline from parsing to code generation.

### Core Components

- **Abstract Syntax Tree (AST)**
  - Located in `lib/Ast/`
  - Handles parsing and syntax representation
  - Includes preprocessor directives and type system

- **Code Generation**
  - Located in `lib/Codegen/`
  - Transforms AST into LLVM IR
  - Manages expression generation and control flow

### Directory Structure

- `app/`: Contains the main compiler entry point
- `lib/`: Houses the core compiler implementation
  - `Ast/Parser/`: Syntax parsing and AST construction
  - `Codegen/ExprGen/`: Expression and control flow generation
  - `Shared/`: Common utilities and helper functions

- `examples/`: Collection of Frost programs showcasing language features
  - `std/`: Standard library implementations
  - Various demonstration programs (donut.ff, mandelbrot.ff, etc.)

- `test/`: Comprehensive test suite
  - Unit tests for parser components
  - Integration tests for code generation
  - Specification tests for language features

### Build System

- Uses Cabal for package management
- Makefile for common development tasks
- Continuous integration scripts in `scripts/`

### Documentation

- `docs/`: Comprehensive documentation
  - User manual for language features
  - Developer documentation for compiler internals
  - Syntax specifications and examples

The project follows a test-driven development approach with extensive unit
testing and continuous integration practices to maintain code quality and
reliability.
