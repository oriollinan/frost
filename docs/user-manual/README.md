# The Frost Programming Language

Elegance meets systems programming in Frost, a modern language designed for
those who believe code should be both powerful and beautiful. Born from the
philosophy that programming is an art form, Frost brings aesthetic clarity to
systems development without compromising on performance or control.

## Why Frost?

Systems programming has traditionally meant wrestling with complex syntax and
making trade-offs between readability and performance. Frost challenges this
notion by offering:

**Beautiful by Design** Write code that speaks for itself. Frost's syntax is
crafted to create visual harmony, making your programs not just functional, but
a joy to read and maintain.

**Systems-level Control** Direct LLVM IR compilation ensures your code runs
close to the metal, giving you the performance you need for systems programming
without sacrificing expressiveness.

**As Fast as C** Frost's performance rivals that of C, making it a powerful
choice for systems programming, high-performance computing, and other
performance-critical applications.

## A Taste of Frost

```frost
import "https://frost-lang.deno.dev/std/io.ff"

main: never -> never = {
    printf("Hello world!\n")
}
```

## Getting Started

Frost programs use the `.ff` file extension and can be compiled using the Frost
compiler, which transforms your beautiful source code into efficient LLVM IR.

To begin your journey with Frost, you'll need:

- The Frost compiler.
- LLVM toolchain.
- Your favorite text editor.

For more details, see the [Getting Started](./getting-started.md) guide.

## Philosophy

We believe that code is read more often than it is written. Every Frost program
is a canvas where developers can express their intentions clearly and
beautifully. The language encourages:

- Clean, readable syntax that reduces cognitive load
- Expressive patterns that make intent clear
- A focus on elegance and aesthetics
- Performance that rivals traditional systems languages

---

_"Programming is not just about making things work—it's about making them work
beautifully."_
