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
    (let ((s (string->symbol (string-append prefix (number->string counter)))))
      (if (symbol-add s)
          s
          (loop (+ counter 1))))))


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

(define (primitive-value? expr)
  (or (null? expr)
      (symbol? expr)
      (number? expr)
      (string? expr)
      (boolean? expr)
      (pattern? '(quote _) expr)))

;; Mutable cells

(define (make-cell v) (cons v '()))
(define (cell-value c) (car c))
(define (set-cell! c v) (set-car! c v))


;; CPS transform [taken from http://matt.might.net/articles/cps-conversion/ - thanks to Matt Might]

(define (aexpr? expr)
  (or (pattern? '(lambda _ . _) expr)
      (primitive-value? expr)))


(define (T-k expr k)
  (cond ((aexpr? expr) (k (M expr)))
        ((pattern? '(begin _) expr)
         (T-k (cadr expr) k))
        ((pattern? '(begin _ . _) expr)
         (T-k (cadr expr) (lambda (_)
                            (T-k `(begin . ,(cddr expr)) k))))
        ((pattern? '(if _ _ _) expr)
         (let* ((rv (gensym 'rv))
                (cont `(lambda(,rv) ,(k rv))))
           (T-c expr cont)))

        ((list? expr)
         (let* ((rv (gensym "rv"))
                (cont `(lambda (,rv) ,(k rv))))
           (T-c expr cont)))
        (error "T-k input language")))


             

(define (T-c expr c)
  (cond ((aexpr? expr) `(,c ,(M expr)))
        ((pattern? '(begin _) expr)
         (T-c (cadr expr) c))
        ((pattern? '(begin _ . _) expr)
         (T-k (cadr expr) (lambda (_)
                            (T-c `(begin . ,(cddr expr)) c))))


        ((pattern? '(if _ _ _) expr)
         (let* ((k (gensym "k")))
           (let ((bool (cadr expr))
                 (thn (caddr expr))
                 (els (cadddr expr)))
             (let ((if-form (lambda (v)
                              (T-k bool (lambda (aexp)
                                          `(if ,aexp 
                                               ,(T-c thn v)
                                               ,(T-c els v)))))))
               (if (symbol? c)
                   (if-form c)
                   (let ((k (gensym 'k)))
                     `((lambda (,k) ,(if-form k)) ,c)))))))

        ((list? expr)
         (let ((f (car expr)) (args (cdr expr)))
           (T-k f (lambda (fk)
                    (T*-k args (lambda (argsk)
                                 `(,fk ,c . ,argsk)))))))
        (error "T-c input language")))

(define (T*-k exprs k)
  (cond ((null? exprs) (k '()))
        ((pair? exprs) (T-k (car exprs) (lambda (hd)
                                          (T*-k (cdr exprs) (lambda (tl)
                                                              (k (cons hd tl)))))))))

(define (M aexpr)
  (cond ((pattern? '(lambda _ _) aexpr)
         (let ((k (gensym "k")) (vars (cadr aexpr)) (body (caddr aexpr)))
           `(lambda (,k . ,vars) ,(T-c body k))))
        ((primitive-value? aexpr) aexpr)
        (else (error "not an aexpr in M!"))))


(define in-code1
  '((lambda ()
   (begin
     (((lambda (r)
         (begin
           ((lambda (f) (begin (f f)))
            (lambda (f) (begin (r (lambda (x) (begin ((f f) x)))))))))
       (lambda (s)
         (begin
           (lambda (l)
             (begin
               (if (null? l)
                 (display 'end)
                 (begin (display (car l)) (s (cdr l)))))))))
      (cons 'foo (cons 'bar (cons 'baz '()))))))))



(define out-code1
'((lambda (_)
   ((lambda (_ r)
      ((lambda (_ f) (f _ f))
       _
       (lambda (_ f)
         (r _ (lambda (_ x) (f (lambda (_) (_ _ x)) f))))))
    (lambda (_)
      (cons (lambda (_)
              (cons (lambda (_)
                      (cons (lambda (_) (_ _ _)) 'foo _))
                    'bar
                    _))
            'baz
            '()))
    (lambda (_ s)
      (_ (lambda (_ l)
              (null? (lambda (_)
                       (if _
                         (display _ 'end)
                         (car (lambda (_)
                                (display
                                  (lambda (_)
                                    (cdr (lambda (_) (s _ _)) l))
                                  _))
                              l)))
                     l))))))
 halt))


(let ((cps-form (T-c in-code1 'halt)))
  (display 'cps) (newline)
  (newline)
  (display (if (pattern?  out-code1 cps-form) "PASSED" "FAILED"))
  (newline)
  (display cps-form)
  (newline))
