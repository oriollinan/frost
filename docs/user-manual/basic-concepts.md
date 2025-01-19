# Basic Concepts in Frost

## Compilation Pipeline

Frost's compilation process is flexible and leverages LLVM's powerful
infrastructure:

**Compilation Options**

```bash
# Generate LLVM IR
frostc -i source.ff -o output.ll

# JIT compilation and immediate execution
frostc -i source.ff | lli

# Generate native object files
frostc -i source.ff | llc -filetype=obj -o output.o

# Direct compilation to executable
frostc -i source.ff | clang -x ir - -o output
```

**Optimization Levels**

Frost's backend is LLVM, which means we inherit the use of the
[`opt`](https//llvm.org/docs/CommandGuide/opt.html) tool for optimization:

```bash
# Apply optimization passes
frostc -i source.ff | opt -O3 -S | lli

# Apply custom optimization pipeline, e.g., mem2reg
frostc -i source.ff | opt -passes=mem2reg -S | lli
```

## Language Fundamentals

All Frost programs must have one `main` function, which is the entry point. It
takes optionally 3 parameters: `argc`, `argv`, and `envp`:

```frost
main: int **byte **byte -> int = argc argv envp {
    % Program logic

    0
}
```

The main function can return an integer value, which is the exit code of the
program. The `argc` parameter is the number of arguments passed to the program,
`argv` is an array of strings containing the arguments, and `envp` is an array
of strings containing the environment variables.

### Module System

Frost uses a clean import syntax with explicit paths, allowing for both relative
and HTTP imports:

```frost
import "./modules/utils.ff" % Relative import

import "https://frost-lang.deno.dev/std/io.ff" % HTTP import
```

If the remote server needs authentication, credentials are accepted as an
environment variable when running the compiler. This will be set as the
`Authorization` header in the request, with a `Bearer` token:

```bash
FROST_PRIVATE_REGISTRY_AUTH="ey..." frostc -i source.ff
```

Frost supports up to `25` levels of nested imports. This limit is arbitrary and
can be increased if needed. The compiler supports both kinds of imports, and
they can be mixed in the same file.

### Standard Library

Frost has a somewhat limited Standard Library, but it provides essential
modules:

- **inet.ff**: Network operations
- **io.ff**: Input/output operations
- **lib.ff**: Core utilities
- **math.ff**: Mathematical functions
- **socket.ff**: Socket operations
- **string.ff**: String manipulation
- **uni.ff**: POSIX system calls
- **opengl.ff**: OpenGL bindings
- **sdl2.ff**: SDL2 bindings

<!-- deno-fmt-ignore -->
{% hint style='info' %}
Standard `C` libraries are linked by default when using `llc`/`clang`/`lli` or 
equivalent. In order to use `OpenGL` and`SDL2` bindings, the corresponding libraries
must be installed on the system.

For example, to compile a program using `SDL2`, you need to link the library
with `clang` or similar:

```bash
frostc -i source.ff | clang -lSDL2 -x ir - -o output
```

{% endhint %}

All of these are available at
[`https://frost-lang.deno.dev/std/`](https://frost-lang.deno.dev/std/).

We also host all examples under the root route of the server. You can access
them at [`https://frost-lang.deno.dev/`](https://frost-lang.deno.dev/).

For example, if you want to render a `3d` ASCII donut, you can import the
`donut.ff` example and run it out of the box:

```frost
import "https://frost-lang.deno.dev/donut.ff"
```

And then JIT-compile and run it:

```bash
frostc -i donut.ff | lli
```

### Variables and Types

Variables in Frost are statically typed. They support implicit conversion out of
the box. They can be either `local` or `global`. The syntax is the same for
both, globals are declared outside of functions:

```frost
amount: int = 2
pi: double = 3,14159

double_pi: double = pi * amount % Implicit conversion
```

Frost also supports pointers and references:

```frost
ptr: *int = amount.& % Reference

% Assignment
ptr.* = 3
```

And arrays and structs:

```frost
% Array declaration
my_array: [8]int = [1 2 3 4 5 6 7 8]
first_elem: int = my_array.#0 % 1

index: int = 2
third_elem: int = my_array.#index % 3

% Struct declaration
hobby :: struct {
    name -> *byte
    hours -> int
}

person :: struct {
    name -> *byte
    age -> int
    passion -> hobby
}

% Struct initialization
me: person

me.name = "Javi"
me.age = 20
me.passion.name = "American Football"
me.passion.hours = 1000

printf(">> %s is %d years old and loves %s\n" me.name me.age me.passion.name)
```

### Control Flow

Frost supports control flow constructs like loops and conditionals.

**Loops**

Frost supports loops and iteration with ranges:

```frost
% Basic loop
loop not false {
    printf(">> Frost is awesome!\n")
}

INIT_VAL: int = 0
STEP_SIZE: int = 2
MAX_ITERS: int = 100

% Iteration with range
from INIT_VAL to MAX_ITERS by STEP_SIZE [i: int] {
    printf(">> Iteration %d\n" i)
}
```

**Conditionals**

Frost supports basic conditional statements. Else branches are optional:

```frost
% Basic conditional

condition: bool = 10 mod 2 is 0

if condition {
    % True branch
    printf(">> Condition is true\n")
} else {
    % False branch
    printf(">> Condition is false\n")
}
```

### Memory Management

Frost provides safe memory management with explicit allocation. The `defer`
keyword is used to ensure cleanup, and will place the cleanup code at the end of
the current scope. It can also be a block:

```frost
import "https://frost-lang.deno.dev/std/lib.ff"

% Allocation with automatic cleanup
buffer: *byte = malloc(size)
defer {
    free(buffer)
    printf(">> Memory freed\n")
}

% Pointer operations
ptr: *byte = buffer + offset   % Pointer arithmetic
buffer.* = value        % Dereferencing

% Deferred blocks will run at the end of the scope
```

### Functions

Functions in Frost can take multiple parameters and return a single value.

```frost
import "https://frost-lang.deno.dev/std/io.ff"

fibonacci: int -> int = n {
    if n < 2 {
        ret n
    }

    fibonacci(n - 1) + fibonacci(n - 2)
}

main: int -> int = argc {
    printf(">> Fibonacci of 10 is %d\n" fibonacci(10))

    0
}
```

Frost also supports a special kind of function: `foreign` functions. These are
functions that are implemented in another language and are linked at runtime:

```frost
printf: foreign *byte *... -> int
```

For instance, this `printf` function is implemented in C and linked at runtime.
It takes a format string and a variable number of arguments, denoted by `...`.

### Type System

**Basic Types**

- `int`: Integer numbers
- `double`: Floating-point numbers
- `byte`: 8-bit values
- `bool`: Boolean values
- `never`: No value, known as `void` in other languages

**Custom Types**

Users are able to use user-defined types out of the box, such as custom width
integers and structs:

```frost
small: int8 = 111 % This is equivalent to `byte`
huge: int128 = 123456789012345678901234567890

packet :: struct {
    size -> int
    data -> *byte
}
```

**Type Casting**

Frost supports explicit type casting with the `@type(expr)` syntax. This is
useful for converting between different types:

```frost
pi: double = 3,14159
rounded: int = @int(pi)
```

This also works with pointers and references:

```frost
raw: *byte = malloc(8)
defer free(raw)

if not raw {
    ret 1
}

buffer: *double = @*double(raw)

% Dereference and assign
buffer.* = 3,14159
```

## Operations

Frost support both unary and binary operations, including arithmetic, bitwise,
logical, and comparison operations. These are just a few examples:

```frost
% Arithmetic
sum: int = 1 + 2
diff: int = 3 - 4

% Bitwise
and: int = 0b1010 & 0b1100
or: int = 0b1010 | 0b1100

% Logical
cond: bool = true and false
neg: bool = not true

% Comparison
eq: bool = 1 is 1
neq: bool = not (1 is 2)
```

## Preprocessor Directives

Another powerful feature of Frost is the preprocessor. It allows for conditional
compilation, macro definitions, and file inclusion.

For instance, you can define a macro for a specific platform in a module:

```frost
sockaddr_in :: struct {
?defined(__APPLE__)
    sin_len -> byte
    sin_family -> byte
?else
    sin_family -> int16
?end
    sin_port -> int16
    sin_addr -> in_addr
    sin_zero -> *int8
}
```

Frost supports macros that are available in the current user's environment. This
means that you can define a macro in the command line and use it in your code.

For example, to enable the `__APPLE__` macro, you can compile the code with:

```bash
__APPLE__=1 frostc -i source.ff -o output.ll
```

This will enable the `__APPLE__` macro in the code, and the compiler will
evaluate the conditionals accordingly, using the correct struct definition.

```frost
import "custom/socket.ff"

main: never -> int = {
    sockaddr: sockaddr_in

    sockaddr.len = @byte(16)
    sockaddr.sin_family = @byte(2)
    % ...
}
```

## Best Practices

- Use meaningful variable names
- Avoid magic numbers
- Always handle memory cleanup with `defer`
- Structure code with clear module organization
- Use comments for complex algorithms

Remember, Frost encourages writing clean, readable code that maintains both
beauty and performance. The syntax is designed to be intuitive while providing
low-level control when needed.
