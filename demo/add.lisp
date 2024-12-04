; Sample LISP code, evaluates to `42`
(define example 21)
(define lambda_0 (lambda (x) (* x 2)))
(define lambda_1 (lambda (y) (- y 1)))

(define $$generated
    (if (= 1 1)
        (lambda_0 example)
        (lambda_1 2)))

($$generated)
