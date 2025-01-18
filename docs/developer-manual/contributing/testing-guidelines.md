Frost uses Hspec with Hspec Discover for its testing framework. This approach
provides a structured and expressive way to write and organize tests for the
Frost compiler.

Hspec is a popular testing framework for Haskell that allows developers to write
human-readable specifications of their code's behavior. Hspec Discover is an
extension that automatically finds and runs all test files in a project,
simplifying test organization and execution.

To use Hspec with Hspec Discover in Frost:

1. Test files are placed in the `test/` directory, mirroring the structure of
   the `lib/` directory.

2. Each test file should end with `Spec.hs`, for example:

   ```
   test/
     Ast/
       Parser/
         ExprSpec.hs
         LiteralSpec.hs
     ...
   ```

3. A main test file (`Spec.hs`) in the `test/` directory uses Hspec Discover:

   ```haskell
   {-# OPTIONS_GHC -F -pgmF hspec-discover #-}
   ```

4. Individual test files use Hspec to describe and test specific components:

   ```haskell
   module Ast.Parser.ExprSpec where

   import Test.Hspec
   import Ast.Parser.Expr

   spec :: Spec
   spec = do
     describe "parseExpr" $ do
       it "parses a simple addition" $ do
         -- Test code here
   ```

5. Run tests using `cabal test`, which will automatically discover and execute
   all test specs.

This setup allows for organized, maintainable, and easily extensible tests as
the Frost compiler evolves.
