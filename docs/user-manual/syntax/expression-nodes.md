# Expression Nodes in Frost

## Literals (Lit)

Direct representation of values:

```frost
42          % Integer literal
3,14159     % Double literal
'A'         % Character literal
true        % Boolean literal
"Hello"     % String literal
```

## Variables (Var)

Named storage locations:

```frost
x: int = 42
counter: int = 0
BUFFER_SIZE: int = 1760  % Constant declaration
```

## Functions

Function declarations with type signatures:

```frost
% Basic function
main: never -> int = {
    % Function body
}

% Function with parameters
process: int double -> never = value coefficient {
    % Function body
}
```

## Declarations

Variable and constant declarations:

```frost
% Variable declarations
x: int = 42
message: *byte = "Hello"

% Global declarations (outside functions)
globalDouble: double = 0.2
globalStruct: structType = structType { hello = 123 }
```

## Assignment

Value assignment operations:

```frost
x = 42              % Direct assignment
ptr.* = value       % Pointer assignment
counter += 1        % Compound assignment
```

## Function Calls (Call)

Invoking functions:

```frost
printf("Value: %d\n" x)
malloc(size)
sin(angle)
```

## Conditional Expressions (If)

Branching logic:

```frost
if condition {
    % True branch
} else {
    % False branch
}
```

## Loops

**While (loop)**

Frost supports loop constructs for iteration:

```frost
loop condition {
    % Loop body
}
```

**From (range)**

Iterating over a range of values, by step:

```frost
from 0 to SET_HEIGHT by 1 [y: int] {
    % Loop body
}
```

## Block Expressions (Block)

Grouped statements:

```frost
{
    temp: int = x
    x = y
    y = temp
}
```

## Control Flow

**Return**

Frost supports implicit and explicit return statements. The `ret` keyword is
used for explicit return.

```frost
ret value    % Alternative syntax
```

The last expression in a function is implicitly returned, without the need for a
`ret` statement.

```
main: never -> int = {
    42
}
```

**Break and Continue**

Frost supports break and continue statements for control flow:

```frost
loop true {
    if condition {
        stop    % Break
    }
    if skip_condition {
        next    % Continue
    }
}
```

## Unary Expressions (Postfix)

Operations after the operand:

```frost
x++         % Post-increment
y--         % Post-decrement
z.&         % Address-of
k.*         % Dereference
```

## Unary Expressions (Prefix)

Operations before the operand:

```frost
!flag       % Logical NOT
not flag    % Bitwise NOT
~value      % Bitwise NOT
++x         % Pre-increment
--y         % Pre-decrement
```

## Operators (Op)

Binary operations, including arithmetic, bitwise, and logical operators:

```frost
a * b       % Multiplication
c / d       % Division
e mod f     % Modulus
g + h       % Addition
i - j       % Subtraction
k & l       % Bitwise AND
m | n       % Bitwise OR
o ^ p       % Bitwise XOR
q << r      % Left shift
s >> t      % Right shift
u is v      % Equality
w == x      % Equality
y != z      % Inequality
a <= b      % Less than or equal
c >= d      % Greater than or equal
e < f       % Less than
g > h       % Greater than
i and j     % Logical AND
k && l      % Logical AND
m or n      % Logical OR
o || p      % Logical OR
```

## Structure Access (StructAccess)

Member access in structures:

```frost
point :: struct {
    x -> double
    y -> double
}

options :: struct {
    enabled -> bool
}

config :: struct {
    settings -> options
    enabled -> bool
}

my_point: point
my_config: config

my_point.x
my_config.settings.enabled
```

## Array Access

Array indexing:

```frost
array: [10]int = [1 2 3 4 5 6 7 8 9 10]

index: int = 2
array.#index   % Accessing array elements
argv.#(argc - 1) % Special syntax for expressions
```

## Type Casting (Cast)

Explicit type conversions:

```frost
@int(floating_value)
@double(integer_value)
@*byte(string_value)
```

Remember that Frost's expression syntax is designed to be clear and unambiguous,
with a focus on readability while maintaining low-level control when needed. The
language uses a consistent style across different expression types to make code
easier to understand and maintain.
