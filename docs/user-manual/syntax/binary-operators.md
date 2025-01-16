## Binary Operators

**Addition (+)**

```frost
% 8
result: int = 5 + 3
```

**Subtraction (-)**

```frost
% 5
result: int = 8 - 3
```

**Multiplication (*)**

```frost
% 12
result: int = 4 * 3
```

**Division (/)**

```frost
% 5.0
result: double = 10 / 2
```

**Modulus (mod)**

```frost
% 1
result: int = 5 mod 2
```

## Bitwise Operators

**AND (&)**

```frost
% 1100 & 1010 = 1000 (8)
result: int = 12 & 10
```

**OR (|)**

```frost
% 1100 | 1010 = 1110 (14)
result: int = 12 | 10
```

**XOR (^)**

```frost
% 1100 ^ 1010 = 0110 (6)
result: int = 12 ^ 10
```

**Left Shift (<<)**

```frost
% 1000 << 2 = 100000 (32)
result: int = 8 << 2
```

**Right Shift (>>)**

```frost
% 1000 >> 1 = 0100 (4)
result: int = 8 >> 1
```

## Comparison Operators

**Equality**

```frost
% Standard equality
result: int = x == y

% Alternative syntax
result: int = x is y
```

**Inequality**

```frost
% Standard inequality
result: int = x != y

% Logical equivalent
result: int = not (x is y)
```

**Less Than**

```frost
result: int = x < y
```

**Greater Than**

```frost
result: int = x > y
```

**Less Than or Equal**

```frost
result: int = x <= y

% Logical equivalent
result: int = not (x > y)
```

**Greater Than or Equal**

```frost
result: int = x >= y

% Logical equivalent
result: int = not (x < y)
```

## Logical Operators

**AND**

```frost
% Python style
result: int = x and y

% C-style
result: int = x && y
```

**OR**

```frost
% Python style
result: int = x or y 

% C-style
result: int = x || y
```
