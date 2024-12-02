; Sample LISP code, evaluates to `3`
(define lambda_0 (lambda (x) (+ x 1)))
(define lambda_1 (lambda (y) (- y 1)))

(define $$generated
    (if (= 1 1)
        (lambda_0 2)
        (lambda_1 2)))

($$generated)
