(define (mangle s)
  (let ((down (lambda (char)
                (cond 
                 ((equal? char #\-) (list #\_))
                 ((equal? char #\?) (list #\_ #\q #\u #\e #\s #\t #\i #\o #\n))
                 ((equal? char #\!) (list #\_ #\b #\a #\n #\g))
                 ((equal? char #\=) (list #\_ #\n #\u #\m #\e #\q))
                 ((equal? char #\<) (list #\_ #\l #\t))
                 ((equal? char #\>) (list #\_ #\g #\t))
                 ((equal? char #\*) (list #\_ #\s #\t #\a #\r))
                 (else (list char))))))
    (let ((mangled (map down (string->list (symbol->string s)))))
      (list->string (foldl append '() mangled)))))

(define (mangle* s n)
  (let ((m (mangle s)))
    (cond ((or (equal? s 'invoke-closure)
               (equal? s 'vector))
           (string-append "scm_" (string-append m (number->string n))))
          ((or (equal? s 'make-closure)
               (equal? s 'vector-ref))
           (string-append "scm_" m))
          (else m))))


(define (formatter formatters)
  (foldl (lambda (m c)
           (cond ((procedure? c) (c m))
                 ((string? c) ((const-formatter c) m))))
         (lambda (ss) (if (null? ss) "" (error "format given too many args")))
         (reverse formatters)))

(define (simple-formatter format-fn)
  (lambda (k)
    (lambda (args)
      (if (null? args)
          (error "format given too few args")
          (begin (display (format-fn (car args)))
                 (k (cdr args)))))))

(define (const-formatter x)
  (lambda (k)
      (lambda (args)
        (display x)
        (k args))))

(define ~% (const-formatter "\n"))

(define (~@ formatters)
  (let ((f (formatter (cons ~a formatters))))
    (lambda (k)
      (lambda (args)
        (if (null? args)
            ""
            (begin
              (foldl (lambda (m c) (f (list m c))) "" (car args))
              (k (cdr args))))))))


(define ~a (simple-formatter tostring))
(define ~m (simple-formatter mangle))
(define ~s (simple-formatter writestring))
