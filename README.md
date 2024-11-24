# glados

```
 __________________________
< what is an s-expression? >
 --------------------------
        \   ^__^
         \  (oo)\_______
            (__)\       )\/\
                ||----w |
                ||     ||
```

# setup

> [!NOTE]
> You only need to run this command once. You also need to have
> [`pre-commit`](https://pre-commit.com/#installation) installed.

This repository uses pre-commit hooks to ensure code quality. To install the
hooks, run the following command:

```bash
pre-commit install
```

Hooks for code formatting and linting will run automatically when you commit
changes. These hooks are `ormolu` and `hlint`. Please make sure to fix any
errors before committing.

## devcontainer

This project includes a `devcontainer` configuration for Visual Studio Code. To
use the devcontainer, you need to have
[Docker](https://docs.docker.com/get-docker/),
[Visual Studio Code](https://code.visualstudio.com/) and the
[Remote - Containers](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
extension installed.

To open the project in the devcontainer, open the command palette
(`Ctrl+Shift+P`) and run the command `Remote-Containers: Reopen in Container`.
All the necessary tools and dependencies will be installed in the container.

# development

To build the project, run the following command:

```bash
cabal build
```

To run the project, run the following command:

```bash
cabal run
```

# testing

After adding a new feature or fixing a bug, please add tests to cover the new
code. To run the tests, run the following command:

```bash
cabal test
```

Follow the test spec to ensure that the tests are correct. We use
[`hspec`](https://hspec.github.io/) and
[`quickcheck`](https://hackage.haskell.org/package/QuickCheck) for testing.

## adding a new test

The test folder, `test`, contains a folder for each logical module in the
project. Inside each module folder, there are files that contain the tests for
that module.

> [!WARNING]
> Test files must end in `Spec.hs` to be picked up by the test runner. For
> example, `ModuleSpec.hs` is a valid test file name.

To add a new test, create a new file in the appropriate module folder. The file
should have the following structure:

```haskell
-- test/ModuleName/ModuleSpec.hs
module ModuleName.ModuleSpec (spec) where

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (property)

spec :: Spec
spec = do
  describe "ModuleName.Module" $ do
    it "should do something" $ do
      property $ \x y -> x + y == y + x
```
