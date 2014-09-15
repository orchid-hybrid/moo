(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdraar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))


;; BOOLEAN

(define (not p) (if p #f #t))


;; LIST OPERATIONS


(define (list? lst)
  (or (null? lst)
      (and (pair? lst)
           (list? (cdr lst)))))

(define (assoc k l)
  (if (null? l)
      #f
      (if (equal? k (caar l))
          (car l)
          (assoc k (cdr l)))))

(define (length lst)
  (define (length-aux lst i)
    (if (null? lst)
        i
        (length-aux (cdr lst) (+ i 1))))
  (length-aux lst 0))

(define (append l m)
  (if (null? l)
      m
      (cons (car l) (append (cdr l) m))))

(define (fold fn init lst)
  (if (null? lst)
      init
      (fn (car lst) (foldr fn init (cdr lst)))))
(define (foldl fn init lst)
  (if (null? lst)
      init
      (foldl fn (fn init (car lst)) (cdr lst))))
(define (foldr fn init lst)
  (if (null? lst)
      init
      (fn (car lst) (foldr fn init (cdr lst)))))

(define (map fn lst)
  (if (null? lst)
      '()
      (cons (fn (car lst)) (map fn (cdr lst)))))
(define (map2 fn lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (fn (car lst1) (car lst2)) (map2 fn (cdr lst1) (cdr lst2)))))

(define (for-each fn lst)
  (if (null? lst)
      '()
      (begin (fn (car lst)) (for-each fn (cdr lst)))))

(define (member elt l)
  (if (null? l) #f (if (equal? elt (car l)) #t (member elt (cdr l)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFICIENT MAP ALGORITHM

(define (tail-map f list cell)
  (if (null? list)
      #f
      (begin (set-cdr! cell (cons (f (car list))
                                  (cdr list)))
             (tail-map f (cdr list) (cdr cell)))))

(define (fast-map f list)
  (if (null? list)
      '()
      (let ((cell (cons (f (car list)) '())))
        (tail-map f (cdr list) cell)
        cell)))

(define (reverse list) (foldl (lambda (y x) ( cons x y)) '() list))




;; EQUAL

(define (equal? p q)
  (cond ((and (procedure? p) (procedure? q)) #f)
        ((and (number? p) (number? q)) (= p q))
        ((and (boolean? p) (boolean? q))
         (or (and p q)
             (and (not p) (not q))))
        ((or (and (symbol? p) (symbol? q))
             (and (char? p) (char? q))
             (and (string? p) (string? q)))
         (eq? p q))
        ((and (null? p) (null? q))
         #t)
        ((and (pair? p) (pair? q))
         (and (equal? (car p) (car q))
              (equal? (cdr p) (cdr q))))
        (#t #f)))

;; ERROR

(define (error e)
  (display (tostring e))
  (newline)
  (exit))


;;;;;;;;
;; IO ;;
;;;;;;;;

(define alphabetic-chars (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define numeric-chars (string->list "1234567890"))
(define (char-alphabetic? c)
  (member c alphabetic-chars))
(define (char-numeric? c)
  (member c numeric-chars))


(define (peek-char port) (peek-char0))

(define (read-char port)
  (let ((c (read-char0)))
    (if (equal? #\newline c)
        (set-cell! (cadr port) (+ (cell-value (cadr port)) 1))
        #f)
    (set-cell! (car port) (+ 1 (cell-value (car port))))
    c))

(define (open-input-file x) x)

(define (wrap-port-with-line-tracking p)
  (let* ((line (make-cell 1))
         (get-line (lambda () (cell-value line))))
    (cons get-line
          (list (make-cell 0) line))))

;; DISPLAY

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

(define (concat-map f l)
  (if (null? l)
      '()
      (append (f (car l))
              (concat-map f (cdr l)))))

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


(define (display obj)
  (put-string (tostring obj)))

(define (string->list s)
  (let ((len (string-length s)))
    (let loop ((l '()) (i 0))
      (if (> i (- len 1))
          (reverse l)
          (loop (cons (string-ref s i) l)
                (+ i 1))))))

(define (list->string s)
  (let ((str (string-make (length s))))
    (let loop ((s s) (i 0))
      (if (null? s)
          str
          (begin
            (string-set! str i (car s))
            (loop (cdr s) (+ 1 i)))))))
