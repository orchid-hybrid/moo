(define (f x)
  (if (null? x) #f (begin (put-string (car x)) (f (cdr x)))))
(let loop ()
  (f '("xxx" "yy" "z" "fwea" wed "erogkroeg" "ok" "ok" "ko"))
  (loop))
