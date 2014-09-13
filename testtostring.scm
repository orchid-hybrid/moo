(define (tostring2 obj)
  (cond
   ((string? obj) obj)
   ((char? obj) (char->string obj))
   ((boolean? obj) (if obj "#t" "#f"))
   ((procedure? obj) "#<procedure>")
   ((number? obj) (number->string obj))
   ((symbol? obj) (symbol->string obj))
   ((null? obj) "()")
   ((list? obj) (foldl string-append "(" (cons
                                           (foldl (lambda (m c) (string-append (string-append m) " " (tostring2 c)))
                                                    (tostring2 (car obj))
                                                    (cdr obj))
                                          
                                          (cons ")" '()))))
   ((pair? obj) (foldl string-append "(" (cons (tostring2 (car obj))
                                               (cons " . "
                                                     (cons (tostring2 (cdr obj))
                                                           (cons ")" '()))))))
   (else "??????")))

(display (tostring2 '(a b c)))
