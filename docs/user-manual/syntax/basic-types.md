# Basic Types in Frost

Frost provides a robust set of primitive types designed for systems programming,
offering both safety and performance.

## Numeric Types

### Int (Integer)

The standard integer type in Frost represents whole numbers:

```frost
age: int = 20
```

**Properties**

- Size: 32 bits (4 bytes)
- Range: -2,147,483,648 to 2,147,483,647
- Default value: 0

### Float (Single-precision)

<!-- deno-fmt-ignore -->
{% hint style='info' %}
One quirk of Frost is the use of commas (`,`) instead of periods for floating-point
numbers.
{% endhint %}

32-bit floating-point numbers following IEEE 754:

```frost
temperature: float = 23,5
```

**Properties**

- Size: 32 bits (4 bytes)
- Precision: ~7 decimal digits
- Range: ±3.4E±38

### Double (Double-precision)

64-bit floating-point numbers for higher precision calculations:

```frost
pi: double = 3,14159265359
```

**Properties**

- Size: 64 bits (8 bytes)
- Precision: ~15-17 decimal digits
- Range: ±1.7E±308

## Character Types

### Char

Represents a single Unicode character:

```frost
letter: char = 'A'
newline: byte = '\n'     % Escape sequences
hex: int8 = '\x20'       % Hexadecimal
```

**Properties**

- Size: 8 bits (1 byte)
- Encoding: UTF-8
- Default value: '\0'

## Boolean Type

### Boolean

Represents logical true/false values:

```frost
is_valid: bool = true
has_error: bool = false

result: bool = is_valid and not has_error
```

**Properties**

- Size: 1 byte
- Values: true, false
- Default value: false

## Special Types

### Never

Represents the absence of a type, commonly used in functions that don't return
values:

```frost
no_op: never -> never = {
    % Do nothing
}
```

## Type Conversion

Frost provides safe type conversion mechanisms:

```frost
% Explicit type casting
x: int = 42
y: double = @double(x)  % Convert 'x' to double

% Numeric promotion
a: int = 5
b: double = 2,5

result: double = a + b  % 'a' is automatically promoted to double
```

## Memory Alignment

Types are automatically aligned for optimal performance:

```frost
example :: struct {
    c -> char,    % 1 byte
    i -> int,     % 8 bytes
    f -> float    % 4 bytes
}
% Struct will be properly padded for alignment
```

Remember that Frost's type system is designed to prevent common programming
errors while maintaining performance. The compiler performs extensive type
checking at compile-time to ensure type safety in your programs.
