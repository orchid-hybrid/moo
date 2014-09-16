
;; Mutable cells

(define (make-cell v) (cons v '()))
(define (cell-value c) (car c))
(define (set-cell! c v) (set-car! c v))



(define (wrap-port-with-line-tracking port)
  (let ((line 1))
    (define (read-char*)
      (let ((c (read-char port)))
        (if (equal? #\newline c)
            (set! line (+ line 1))
            #f)
        c))
    (define (char-ready?*)
      (char-ready port))
    (define (close*)
      (close port))
    (define (peek-char*)
      (peek-char port))
    (cons (lambda () line)
          (make-input-port read-char* char-ready?* close* peek-char*))))


(define char->string string)

(define (concat-map f l)
  (if (null? l)
      '()
      (append (f (car l))
              (concat-map f (cdr l)))))


(define (map2 fn lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (fn (car lst1) (car lst2)) (map2 fn (cdr lst1) (cdr lst2)))))

(define (tostring obj)
  (cond
   ((string? obj) obj)
   ((char? obj) (char->string obj))
   ((boolean? obj) (if obj "#t" "#f"))
   ((procedure? obj) "#<procedure>")
   ((number? obj) (number->string obj))
   ((symbol? obj) (symbol->string obj))
   ((null? obj) "()")
   ((list? obj)
    (foldl
     string-append
     "("
     (append
      (list (foldl (lambda (m c) (string-append m (string-append " " (tostring c))))
                   (tostring (car obj))
                   (cdr obj)))
      (cons ")" '()))))
   ((pair? obj) (foldl string-append "(" (cons (tostring (car obj))
                                               (cons " . "
                                                     (cons (tostring (cdr obj))
                                                           (cons ")" '()))))))))


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
