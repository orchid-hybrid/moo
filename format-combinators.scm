(define (down khar)
  (cond 
   ((equal? khar #\-) (list #\_))
   ((equal? khar #\+) (list #\_ #\p #\l #\u #\s #\_))
   ((equal? khar #\~) (list #\_ #\t #\i #\l #\d #\e #\_))
   ((equal? khar #\@) (list #\_ #\a #\t #\_))
   ((equal? khar #\%) (list #\_ #\p #\p #\_))
   ((equal? khar #\?) (list #\_ #\q #\u #\e #\s #\t #\i #\o #\n))
   ((equal? khar #\!) (list #\_ #\b #\a #\n #\g))
   ((equal? khar #\=) (list #\_ #\n #\u #\m #\e #\q))
   ((equal? khar #\<) (list #\_ #\l #\t))
   ((equal? khar #\>) (list #\_ #\g #\t))
   ((equal? khar #\*) (list #\_ #\s #\t #\a #\r))
   (else (list khar))))

(define (mangle s)
  (let ((mangled (map down (string->list (symbol->string s)))))
    (list->string (foldl append '() mangled))))

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

