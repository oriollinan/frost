## Logical Operators

**NOT (!, not)**

```frost
% C-style
result: int = !x

% Python style
result: int = not x
```

## Bitwise Operators

**Bitwise NOT (~)**

```frost
% 1010 becomes 0101
result: int = ~x
```

## Memory Operations

**Dereference (.*)**

```frost
ptr: *int = value.&

% Get value at ptr
result: int = ptr.*
```

**Address-of (&)**

```frost
value: int = 42

% Get address of value
ptr: *int = value.&
```

## Increment/Decrement

**Pre-increment (++x)**

```frost
% Increment before use
++x

% Equivalent operation
x += 1
```

**Pre-decrement (--x)**

```frost
% Decrement before use
--x

% Equivalent operation
x -= 1
```

**Post-increment (x++)**

```frost
% Use before increment
x++
```

**Post-decrement (x--)**

```frost
% Use before decrement
x--
```

## Example Differences

**Pre vs Post Increment**

```frost
x = 42

% Pre-increment: Increment x before its value is used
result = ++x  % result is 43, x is now 43

% Post-increment: Increment x after its value is used
result = x++  % result is 43, x is now 44
```

```frost
x = 42

% Pre-decrement: Decrement x before its value is used
result = --x  % result is 41, x is now 41

% Post-decrement: Decrement x after its value is used
result = x--  % result is 41, x is now 40
```
