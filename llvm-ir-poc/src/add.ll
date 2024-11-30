; ModuleID = 'add.ll'
target triple = "arm64-apple-macosx15.0.0"

; A simple function to add two integers
define i32 @add(i32 %a, i32 %b) {
entry:
  %sum = add i32 %a, %b
  ret i32 %sum
}
