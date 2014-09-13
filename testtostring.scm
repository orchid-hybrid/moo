(define (tostring2 obj)
  (let ((out
  (cond
   ((string? obj) obj)
   ((char? obj) (char->string obj))
   ((boolean? obj) (if obj "#t" "#f"))
   ((procedure? obj) "#<procedure>")
   ((number? obj) (number->string obj))
   ((symbol? obj) (symbol->string obj))
   ((null? obj) "()")
   ((list? obj) (foldl string-append "(" (cons
                                           (foldl (lambda (m c)
                                                    (string-append m
                                                                   (string-append " "(tostring2 c))))
                                                    (tostring2 (car obj))
                                                    (cdr obj))
                                          
                                           (cons ")" '()))))
   ((pair? obj) (foldl string-append "(" (cons (tostring2 (car obj))
                                               (cons " . "
                                                     (cons (tostring2 (cdr obj))
                                                           (cons ")" '()))))))
   (else "??????"))))
    (if (string? out)
        out
        (display "WTF\n"))))

(display (tostring2 '(a b c)))
