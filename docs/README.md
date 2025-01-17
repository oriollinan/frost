# Frost

**Frost** is a modern systems programming language that combines the speed of C
with modern language features. It offers static typing and compiles to
[LLVM IR](https://llvm.org/docs/LangRef.html), delivering high performance while
remaining accessible and easy to learn. Whether you're building system tools or
performance-critical applications, Frost provides the low-level control you need
with the convenience of contemporary programming practices.

## Features

- **Static Typing**: Frost is `statically` typed, catching errors at compile
  time rather than runtime.
- **Modern Syntax**: Frost's syntax is clean and expressive, making it easy to
  read and write.
- **LLVM Backend**: Frost compiles to `LLVM` IR, providing high performance and
  compatibility with a wide range of platforms and toolchains.
- **QOL Features**: Frost includes modern conveniences like `defer` for resource
  management and interoperability with anything that speaks `C`.

## Example

```frost
import "https://frost-lang.deno.dev/std/io.ff"

main: never -> int = {
    printf("Hello, world!\n")

    0
}
```
