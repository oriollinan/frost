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

This should output `42`, which is the result of the `scheme` code.

## scheme

We currently have this code, that has global variables, lambdas, conditionals,
arithmetic operations, local variables, and function calls.

```scm
; Sample scheme code, evaluates to `42`

(define value 21)

(define $$generated
  (if 1
   ((lambda (x)
  (define example 2)
  (* x example))
    value)
   ((lambda (y)
  (- y 1))
    2)))

($$generated)
```

## haskell

The `AST` equivalent, for the moment, is hardcoded in the entrypoint as follows:

```haskell
T.AST
  [ T.Define "value" (T.Lit (T.LInt 21)),
    T.Define
      "$$generated"
      ( T.If
          (T.Lit (T.LInt 1))
          ( T.Call
              ( T.Lambda
                  ["x"]
                  [ T.Define "example" (T.Lit (T.LInt 2)),
                    T.Op T.Mult (T.Var "x") (T.Var "example")
                  ]
              )
              [T.Var "value"]
          )
          ( T.Call
              ( T.Lambda ["y"] [T.Op T.Sub (T.Var "y") (T.Lit (T.LInt 1))]
              )
              [T.Lit (T.LInt 2)]
          )
      )
  ]
```

This `AST` is then converted to the intermediate representation, and then
compiled to an executable, by calling the `$$generated` function. For the
moment, the prototype of this function is hardcoded as `int $$generated(void)`,
but this will be changed in the future.

## llvm

The generated intermediate representation is as follow:

```ll
; ModuleID = '$$generated'

@value = global i32 21

define external ccc  i32 @lambda_0(i32  %x_0) {
  %1 = mul   i32 %x_0, 2 
  ret i32 %1 
}

define external ccc  i32 @lambda_1(i32  %y_0) {
  %1 = sub   i32 %y_0, 1 
  ret i32 %1 
}

define external ccc  i32 @$$generated() {
; <label>:0:
  %1 = icmp ne i32 1, 0 
  br i1 %1, label %then_0, label %else_0 
then_0:
  %2 = load   i32, i32* @value  
  %3 =  call ccc  i32  @lambda_0(i32  %2)  
  br label %merge_0 
else_0:
  %4 =  call ccc  i32  @lambda_1(i32  2)  
  br label %merge_0 
merge_0:
  %5 = phi i32 [%3, %then_0], [%4, %else_0] 
  ret i32 %5 
}
```
