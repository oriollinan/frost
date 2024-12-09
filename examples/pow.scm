(define (pow x n) (
    if (< n 2)
        (x)
        (* x (pow x (- n 1)))
))
(define $$generated (pow 2 3))
