(define x 7)
(define (f y) (+ y 3))
(display (f x))
(letrec ((fib (lambda (n)
                (if (or (= n 2) (< n 2))
                    1
                    (+ (fib (- n 1)) (fib (- n 2)))))))
  (display (fib 20)))
