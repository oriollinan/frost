# demo

> [!NOTE]
> This command must be run from the root of the repository. When done, come back
> to this directory to run the commands below.

First, generate the intermediate representation of the `ast` code. This should
generate a file called [`generated.ll`](./generated.ll) in the `demo` directory.

```sh
cabal run
```

Next, compile the intermediate representation to an executable. This will
generate all object files in the `demo` directory, and link them into an
executable called `generated`.

```sh
make generated
```

Finally, run the executable.

```sh
./generated
```

This should output `5`, which is the result of the `lisp` code.

## lisp

We currently have this code, that has a conditional branch that evaluates to
`true` always, and then adds `2` and `3` together.

```lisp
; Sample LISP code, evaluates to `5`
(if 1 (+ 2 3) (* 4 5))
```

## haskell

The `AST` equivalent, for the moment, is hardcoded in the entrypoint as follows:

```haskell
T.Expr
  ( T.If
      (T.Lit (T.LInt 1))
      (T.Op T.Add (T.Lit (T.LInt 2)) (T.Lit (T.LInt 3)))
      (T.Op T.Multiply (T.Lit (T.LInt 4)) (T.Lit (T.LInt 5)))
  )
```

This `AST` is then converted to the intermediate representation, and then
compiled to an executable, by calling the `$$generated` function. For the
moment, the prototype of this function is hardcoded as `int $$generated(void)`,
but this will be changed in the future.

## llvm

The generated intermediate representation is as follow:

```ll
; ModuleID = 'generated'

define external ccc i32 @$$generated() {
; <label>:0:
  %1 = icmp ne i32 1, 0 
  br i1 %1, label %then_0, label %else_0 
then_0:
  %2 = add   i32 2, 3 
  br label %merge_0 
else_0:
  %3 = mul   i32 4, 5 
  br label %merge_0 
merge_0:
  %4 = phi i32 [%2, %then_0], [%3, %else_0] 
  ret i32 %4 
}
```
