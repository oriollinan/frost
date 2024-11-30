# llvm-ir-poc

This project shows a small proof of concept of how to generate LLVM IR code. The
example function just adds two numbers.

```ll
; ModuleID = 'add.ll'
target triple = "arm64-apple-macosx15.0.0"

; A simple function to add two integers
define i32 @add(i32 %a, i32 %b) {
entry:
  %sum = add i32 %a, %b
  ret i32 %sum
}
```

## dependencies

We could use [`llvm-hs-pure`](https://hackage.haskell.org/package/llvm-hs-pure)
to generate the IR code. This package is a pure Haskell implementation of the
LLVM IR. It is a good starting point to understand how to generate LLVM IR code.

## references

- https://danieljharvey.github.io/posts/2023-02-08-llvm-compiler-part-1.html
- https://llvm.org/docs/LangRef.html
- https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html
- https://llvm.org/docs/tutorial/LangImpl01.html
- https://lukelau.me/kaleidoscope/
- https://github.com/andrescollares/kaleidoscope
