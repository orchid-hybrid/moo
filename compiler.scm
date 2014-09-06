;; Pattern matching

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


;; Desugaring macros





;; Recursion on sexp lambda terms

(define (primitive-value? expr)
  (or (null? expr)
      (symbol? expr)
      (number? expr)
      (string? expr)
      (boolean? expr)))

(define (traverse primitive-case var-case lam-case app-case)
  (lambda (exp)
    (cond ((primitive-value? exp) primitive-case)
          ((symbol? exp) (var-case exp))
          ((pattern? '(lambda _ _) exp)
           (let ((args (cadr exp)) (body (caddr exp)))
             (lam-case args body)))
          ((list? exp) (app-case exp))
          (else (error "not a thing: " exp)))))

;; CPS transform [taken from http://matt.might.net/articles/cps-conversion/ - thanks to Matt Might]

(define (aexpr? expr)
  (or (pattern? '(lambda _ . _) expr)
      (primitive-value? expr)))

(define prims '(+ - / *  =))

(define (prim? term) (member term prims))

(define (T-k expr k)
  (cond ((aexpr? expr) (k (M expr)))
        ((pattern? '(begin _) expr)
         (T-k (cadr expr) k))
        ((pattern? '(begin _ . _) expr)
         (T-k (cadr expr) (lambda (_)
                            (T-k `(begin . ,(cddr expr)) k))))
        ((pattern? '(if _ _ _) expr)
         (let* ((rv (gensym "rv"))
                (cont `(lambda (,rv) ,(k rv))))
           (let ((bool (cadr expr))
                 (thn (caddr expr))
                 (els (cadddr expr)))
             (T-k bool (lambda (aexp)
                         `(if ,aexp
                              ,(T-c thn cont)
                              ,(T-c els cont)))))))
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
             `((lambda (,k)
                 ,(T-k bool (lambda (aexp)
                              `(if ,aexp
                                   ,(T-c thn k)
                                   ,(T-c els k)))))
               ,c))))
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


;; closure conversion

(define (free! free-vars v)
  (let* ((frees (cell-value free-vars))
         (len (length frees))
         (existing (assoc v frees)))
    (if existing
        (cadr existing)
        (begin (set-cell! free-vars (append (list (list v len)) frees))
               `(vector-ref env ,len)))))

(define (closure-convert bound free-vars e)
  (cond ((symbol? e)
         ;; var
         (if (member e bound)
             e
             (free! free-vars e)))
        ((primitive-value? e) e)
        ((pattern? '(if _ _ _) e)
         (let ((b (cadr expr)) (thn (caddr expr)) (els (cadddr expr)))
           `(if ,b
                ,(closure-convert bound free-vars thn)
                ,(closure-convert bound free-vars els))))
        ((pattern? '(lambda _ _) e)
         (let ((vs (if (list? (cadr e)) (cadr e) (list (cadr e)))) (m (caddr e)))
           ;; lam
           ;; TODO trouble here [vs] with varargs
           (let ((inner-free-vars (make-cell '())))
             `(make-closure (lambda ,(cons 'env vs)
                              ,(closure-convert vs inner-free-vars m))
                            (vector . ,(map (lambda (a) (closure-convert bound free-vars a))
                                            (cell-value inner-free-vars)))))))
        ((list? e)
         ;; app
         `(invoke-closure . ,(map (lambda (a) (closure-convert bound free-vars a)) e)))))


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
         (let ((b (cadr expr)) (thn (caddr expr)) (els (cadddr expr)))
           `(if ,b
                ,(hoist thn)
                ,(hoist els))))
        ((pattern? '(lambda _ _) e)
         (let ((vs (cadr e)) (m (caddr e)))
           (hoist! `(lambda ,vs ,(hoist m)))))
        ((list? e)
         (map hoist e))
        (else "unknown in hoist")))

;; C

(define (concat lists)
  (if (null? lists)
      '()
      (append (car lists) (concat (cdr lists)))))

(define (c-gen def)
  (if (pattern? '(define _ (lambda (env . _) _)) def)
      (let ((name (cadr def)) (args (cdadr (caddr def))) (body (caddr (caddr def))))
        `(define-code ,name . ,(append (c-gen-pop-args '() args) (c-gen-body body))))
      (error "malformed definition")))

(define (c-gen-pop-args acc args)
  (if (null? args)
      acc
      (c-gen-pop-args (cons `(pop ,(car args)) acc) (cdr args))))

(define (c-gen-body body)
  (cond ((symbol? body) (list `(push ,body)))
        ((pattern? '(vector-ref env _) body)
         (let ((i (caddr body)))
           (list `(push (ref env ,i)))))
        ((pattern? '(make-closure _ _) body)
         ...)
        ((pattern? '(invoke-closure _ . _) body)
         (let ((continuation (cadr body))
               (arguments (cddr body)))
           (append (concat (map c-gen-body arguments))
                   (c-gen-body continuation))))
        (else (display (list 'c-gen-body body)) (newline)
              (error "error in c-gen-body"))))


;; Compiler

(define (compile form)
  (let ((bound-variables (append '(halt) prims)))
    
    (display 'compiling) (newline)
    (pretty-print form) (newline)
    (let ((cps-form (T-c form 'halt)))
      (display 'cps) (newline)
      (pretty-print cps-form) (newline)
      (let ((cc-form (closure-convert bound-variables (make-cell '()) cps-form)))
        (display 'cc) (newline)
        (pretty-print cc-form) (newline)
        (let ((hoisted-form (hoist cc-form)))
          (display 'hoist) (newline)
          (for-each (lambda (i)
                      (pretty-print `(define ,(car i) ,(cdr i))))
                    lambdas) (newline)
          (pretty-print hoisted-form) (newline)
          (let ()
            (display 'c-gen) (newline)
            (for-each (lambda (i)
                        (display (c-gen `(define ,(car i) ,(cdr i)))) (newline))
                      lambdas)) (newline)
          #t)))))

;;(compile '(lambda (x) (+ x x)))

(compile '(lambda (f x) (f (f (f x)))))

;; testing

(define (cps f)
  (lambda args
    ((car args) (apply f (cdr args)))))

(define (make-closure code env) (cons code env))
(define (invoke-closure closure . args) (apply (car closure) (cons (cdr closure) args)))

(define *k (make-closure (lambda (k env x y) (invoke-closure k (* x y))) (vector)))
(define +k (make-closure (lambda (k env x y) (invoke-closure k (+ x y))) (vector)))
(define halt (make-closure (lambda (r env) (display r)) (vector)))

;; N.B. made env come first

;; (compile '(+ 1 (* 2 3))))

;; (define lambda246
;;   (lambda (rv245 env)
;;     (invoke-closure (vector-ref env 0) (vector-ref env 1) 1 rv245)))

;; (invoke-closure *k (make-closure lambda246 (vector +k halt)) 2 3)


;; (compile '(+ ((lambda (x) (* x (* x x))) 7) 3))

;; (define lambda246
;;   (lambda (rv245 env)
;;     (invoke-closure
;;       (vector-ref env 0)
;;       (vector-ref env 1)
;;       (vector-ref env 2)
;;       rv245)))
;; (define lambda247
;;   (lambda (k244 env x)
;;     (invoke-closure
;;       (vector-ref env 0)
;;       (make-closure lambda246 (vector (vector-ref env 1) k244 x))
;;       x
;;       x)))
;; (define lambda248
;;   (lambda (rv243 env)
;;     (invoke-closure (vector-ref env 0) (vector-ref env 1) rv243 3)))

;; (invoke-closure
;;   (make-closure lambda247 (vector *k *k))
;;   (make-closure lambda248 (vector +k halt))
;;   7)


