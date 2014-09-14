;; This implements gensym, a fresh name generator
;; it stores all the symbols it's previously created
;; and makes sure it only hands out something never
;; seen before

(define symbol-table '())

(define (symbol-add s)
  ;; Add the symbol s to the symbol-table
  ;; unless it's already in there.
  ;; Returns a #t if the symbol is fresh and
  ;; had to be added, #f if it was already in
  ;; there.
  (if (member s symbol-table)
      #f
      (begin
        (set! symbol-table (cons s symbol-table))
        #t)))

(define (add-all-symbols sexp)
  ;; traverse an s-expression adding every symbol in the tree
  (cond ((symbol? sexp) (symbol-add sexp))
        ((pair? sexp)
         (add-all-symbols (car sexp))
         (add-all-symbols (cdr sexp)))
        (else #f)))

(define (gensym prefix)
  (let loop ((counter 0))
    (let ((s (string->symbol (string-append (tostring prefix) (number->string counter)))))
      (if (symbol-add s)
          s
          (loop (+ counter 1))))))


(define (primitive-value? expr)
  (or (null? expr)
      (symbol? expr)
      (number? expr)
      (string? expr)
      (boolean? expr)
      (pattern? '(quote _) expr)))


;; Pattern matching

(define (any p l)
  (if (null? l)
      #f
      (if (p (car l))
          #t
          (any p (cdr l)))))

(define (conj a b) (lambda (x) (and (a x) (b x))))
(define (disj a b) (lambda (x) (or (a x) (b x))))

(define (pattern? p e)
  (cond ((null? p) (null? e))
        ((equal? p '_) #t)
        ((symbol? p) (and (symbol? e) (equal? p e)))
        ((procedure? p) (p e))
        (else (and (pair? e)
                   (pattern? (car p) (car e))
                   (pattern? (cdr p) (cdr e))))))

;; Mutable cells

(define (make-cell v) (cons v '()))
(define (cell-value c) (car c))
(define (set-cell! c v) (set-car! c v))


;; Hoist

(define lambdas '())
(define (hoist! lam)
  (let ((nm (gensym "lambda")))
    (set! lambdas (append lambdas (list (cons nm lam))))
    nm))

(define (hoist e)
  (cond ((symbol? e) e)
        ((primitive-value? e) e)
        ((pattern? '(if _ _ _) e)
         (let ((b (cadr e)) (thn (caddr e)) (els (cadddr e)))
           `(if ,b
                ,(hoist thn)
                ,(hoist els))))
        ((pattern? '(lambda _ _) e)
         (let ((vs (cadr e)) (m (caddr e)))
           (hoist! `(lambda ,vs ,(hoist m)))))
        ((list? e)
         (map hoist e))
        (else "unknown in hoist")))
