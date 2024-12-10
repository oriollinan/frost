(define too (lambda (y) (+ y (+ y y))))
(define crazy (lambda (x) (* x (* x x))))
(define $$generated (
     ((lambda (z) (
                 + (too z) (crazy z)
                 )) 2)
 ))
