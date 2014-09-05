;; Pattern matching
 
(define (pattern? p e)
  (cond ((null? p) (null? e))
        ((equal? p '_) #t)
        ((symbol? p) (and (symbol? e) (equal? p e)))
        (else (and (pair? e)
                   (pattern? (car p) (car e))
                   (pattern? (cdr p) (cdr e))))))


