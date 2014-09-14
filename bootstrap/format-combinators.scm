(define char->string string)
(define (foldl fn init lst)
  (if (null? lst)
      init
      (foldl fn (fn init (car lst)) (cdr lst))))
(define (concat-map f l)
  (if (null? l)
      '()
      (append (f (car l))
              (concat-map f (cdr l)))))

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

(define (escape-string s)
  (let ((escape-char (lambda (c)
                       (if (or (eq? c #\")
                               (eq? c #\\)
                               (eq? c #\?)
                               (eq? c #\'))
                           (list #\\ c)
                           (if (eq? c #\newline)
                               (list #\\ #\n)
                               (list c))))))
    (list->string (concat-map escape-char (string->list s)))))

(define (string-quote s)
  (string-append "\"" (string-append (escape-string s) "\"")))

(define (tostring obj)
  (cond
   ((string? obj) obj)
   ((char? obj) (char->string obj))
   ((boolean? obj) (if obj "#t" "#f"))
   ((procedure? obj) "#<procedure>")
   ((number? obj) (number->string obj))
   ((symbol? obj) (symbol->string obj))
   ((null? obj) "()")
   ((list? obj) (foldl string-append "(" (append
                                          (map (lambda (s) (string-append (tostring s) " ")) obj)
                                          (cons ")" '()))))
   ((pair? obj) (foldl string-append "(" (cons (tostring (car obj))
                                               (cons " . "
                                                     (cons (tostring (cdr obj))
                                                           (cons ")" '()))))))))

(define (writestring obj)
  (cond
   ((string? obj) (string-quote obj))
   ((char? obj) (string-append "#\\" (char->string obj)))
   ((list? obj) (foldl string-append "(" (append
                                           (list (foldl (lambda (m c) (string-append m " " (writestring c)))
                                                  (writestring (car obj))
                                                  (cdr obj)))
                                          
                                          (cons ")" '()))))
   ((pair? obj) (foldl string-append "(" (cons (writestring (car obj))
                                               (cons " . "
                                                     (cons (writestring (cdr obj))
                                                           (cons ")" '()))))))
   (else (tostring obj))))

(define (formatter formatters)
  (let ((fmtr (foldl (lambda (m c)
                       (cond ((procedure? c) (c m))
                             ((string? c) (lambda (ss me)
                                            (display c)
                                            (m ss "")))))
                     (lambda (ss m) (if (null? ss) m (error "format given too many args")))
                     (reverse formatters))))
    (lambda (ss) (fmtr ss ""))))

(define (simple-formatter format-fn)
  (lambda (k)
    (lambda (ss m)
      (if (null? ss)
          (error "format given too few args")
          (begin (display (format-fn (car ss)))
                 (k (cdr ss) ""))))))

(define ~%
  (lambda (k)
      (lambda (ss m)
        (display "\n")
        (k ss ""))))

(define (~@ formatters)
  (let ((f (formatter (cons ~a formatters))))
    (lambda (k)
      (lambda (ss m)
        (k (cdr ss) (foldl (lambda (m c) (f (list m c))) m (car ss)))))))
            

(define ~a (simple-formatter tostring))
(define ~m (simple-formatter mangle))
(define ~s (simple-formatter writestring))

;; (display ((formatter (list ~a ~s (~@ (list ~a)) ~s "hi" ~m ~%)) (list  "q"  "q" (list "z") #\q 'hi>there)))
