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
  (display "\n")
  (exit))



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
   ((list? obj) (foldl string-append "(" (append
                                          (map (lambda (s) (string-append (tostring s) " ")) obj)
                                          (cons ")" '()))))
   ((pair? obj) (foldl string-append "(" (cons (tostring (car obj))
                                               (cons " . "
                                                     (cons (tostring (cdr obj))
                                                           (cons ")" '()))))))))
(define (display obj)
  (put-string (tostring obj)))

(define (newline) (display "\n"))


