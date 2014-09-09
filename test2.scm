(define (y r)
  ((lambda (f) (f f))
   (lambda (f) (r (lambda (x) ((f f) x))))))

((y
  (lambda (s)
    (lambda (l)
      (if (null? l) (display 'end)
          (begin (display (car l)) (s (cdr l)))))))
 '(foo bar baz quux a b c d e f g h i z e e e e o o u o))
