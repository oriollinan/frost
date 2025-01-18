# Coding Standards for Frost Development

Frost is developed using Haskell, leveraging its powerful type system and
functional programming paradigms. Adhering to these coding standards ensures
consistency, readability, and maintainability across the codebase.

## General Principles

1. Write idiomatic Haskell code
2. Prioritize readability and maintainability
3. Leverage the type system for safety and expressiveness
4. Avoid code duplication
5. Prefer pure functions and immutable data structures

## Specific Guidelines

### Naming Conventions

- Use camelCase for function and variable names
- Use PascalCase for type and constructor names
- Prefix type class instances with the type name

```haskell
data UserProfile = UserProfile { userName :: String, userAge :: Int }

class Printable a where
  toString :: a -> String

instance Printable UserProfile where
  toString (UserProfile name age) = name ++ " (" ++ show age ++ ")"
```

### Function Definitions

- Provide type signatures for all top-level functions
- Use pattern matching when appropriate
- Prefer point-free style when it enhances readability

```haskell
-- Good
isAdult :: UserProfile -> Bool
isAdult = (>= 18) . userAge

-- Avoid
isAdult' :: UserProfile -> Bool
isAdult' profile = userAge profile >= 18
```

### Error Handling

- Use `Either` for recoverable errors
- Use `Maybe` for optional values
- Avoid partial functions; use total functions with `Maybe` or `Either` instead

```haskell
-- Good
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Avoid
unsafeHead :: [a] -> a
unsafeHead (x:_) = x
unsafeHead [] = error "Empty list"
```

### Imports

- Use qualified imports for modules with name clashes
- Group and organize imports logically

```haskell
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad (when, unless)
import Control.Applicative ((<|>))
```

### Code Organization

- Keep functions small and focused
- Use meaningful module names that reflect their contents
- Organize related functions into modules

### Comments and Documentation

- Write Haddock comments for all public functions
- Use inline comments sparingly, preferring self-documenting code
- Document complex algorithms or non-obvious implementations

```haskell
-- | Calculates the factorial of a given number.
--   Returns Nothing for negative inputs.
factorial :: Integer -> Maybe Integer
factorial n
  | n < 0     = Nothing
  | n == 0    = Just 1
  | otherwise = Just $ product [1..n]
```

### Performance Considerations

- Use appropriate data structures (e.g., `Map` for associative arrays)
- Be mindful of lazy evaluation and space leaks
- Profile and optimize only when necessary

### Testing

- Write unit tests for all public functions
- Use property-based testing with QuickCheck where applicable
- Aim for high test coverage, but prioritize meaningful tests over coverage
  percentage

By following these coding standards, we ensure that the Frost codebase remains
clean, efficient, and maintainable. Remember, these guidelines are not
exhaustive, and good judgment should always be applied when writing code.
