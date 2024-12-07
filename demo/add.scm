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
