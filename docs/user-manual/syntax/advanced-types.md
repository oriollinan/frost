# Advanced Types in Frost

## Mutable (Mutable References)

All variables in Frost are mutable by default:

```frost
% Mutable reference
value: int = 42

% Mutating the value
value = 24

% Mutable reference to a pointer
ptr: *int = value.&

% Mutating the pointer value
ptr.* = 42
```

## Pointer (Raw Pointers)

Pointers provide direct memory access for systems programming:

```frost
import "https://frost-lang.deno.dev/std/lib.ff"

% Pointer declaration
raw: int = 42
ptr: *int = raw.&           % Pointer to raw memory

data: *byte = malloc(8)     % Raw memory pointer
defer free(data)            % Cleanup

% Pointer operations
ptr.* = 42                  % Dereferencing
offset: *byte = ptr + 1     % Pointer arithmetic
```

**Safety Features**

```frost
% Null checking
if not ptr {
    ret 1
}
```

## Array (Fixed-size Arrays)

Arrays in Frost are fixed-size collections with zero-cost abstractions:

```frost
% Array declaration
numbers: [int]5 = [1 2 3 4 5]

first = numbers.#0          % Accessing array elements
```

## Function (Function Types)

Functions are first-class citizens in Frost. Frost supports higher-order
functions:

```frost
sum_one: int -> int = n {
    n + 1
}

apply: (int -> int) -> int = fn {
    fn(2) + fn(3)
}

main: never -> int = {
    apply(sum_one) % 3 + 4 = 7
}
```

Remember that while these advanced types provide powerful capabilities for
systems programming, they should be used judiciously. Frost's type system helps
prevent common mistakes, but it's important to understand the implications of
using these features, especially regarding memory safety and performance.
