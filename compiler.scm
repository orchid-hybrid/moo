;; Pattern matching

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


;; Desugaring macros

(define (desugar exp)
  (cond
   ((pattern? '(set! _ _) exp) `(set! ,(cadr exp) ,(desugar (caddr exp))))
 
   ((pattern? '(begin . _) exp)
    (cons 'begin (map desugar (cdr exp))))
   
   ((pattern? '(lambda _ . _) exp)
    (let ((params (cadr exp))
          (body (cddr exp)))
      `(lambda ,params ,(desugar (cons 'begin body)))))

   ((pattern? `(let ,symbol? ,list? . _) exp)
    (let* ((name (cadr exp))
           (bindings (caddr exp))
	   (params (map car bindings))
	   (values (map cadr bindings))
	   (body (cdddr exp)))
      (desugar `((y-combinator (lambda (,name) (lambda ,params . ,body))) . ,values))))
   
   ((pattern? `(let ,list? . _) exp)
    (let* ((bindings (cadr exp))
	   (params (map car bindings))
	   (values (map cadr bindings))
	   (body (cddr exp)))
      (desugar `((lambda ,params . ,body) . ,values))))
   
   ((pattern? `(let* ,list? . _) exp)
    (let* ((bindings (cadr exp))
	   (body (cddr exp)))
      (desugar
       (append `(let (,(car bindings)))
               (if (null? (cdr bindings))
                   body
                   `((let* ,(cdr bindings) . ,body)))))))

   ((pattern? `(define _ _ . _) exp)
    (let ((name (caadr exp))
	  (params (cdadr exp))
	  (body (cddr exp)))
      `(define ,name ,(desugar `(lambda ,params . ,body)))))
   
   ((pattern? `(if _ _ _) exp)
    (let ((b (cadr exp))
	  (then (caddr exp))
	  (else (cadddr exp)))
      `(if ,(desugar b)
           ,(desugar then)
           ,(desugar else))))
   
   ((pattern? '(cond . _) exp)
    (cond
     ;; (cond)
     ((equal? '(cond) exp) '())
     ;; (cond (else b))
     ((and (equal? 'else (caadr exp))
	   (= 2 (length exp)))
      (desugar (cons 'begin (cdadr exp))))
     ;; (cond (a b))
     ((= 2 (length exp))
      (desugar `(if ,(caadr exp)
                    ,(cons 'begin (cdadr exp))
                    '())))
     ;; (coond (a b) rest ...)
     ((> (length exp) 2)
      (desugar `(if ,(caadr exp)
                    ,(cons 'begin (cdadr exp))
                    (cond . ,(cddr exp)))))))
   
   ((pattern? '(and . _) exp)
    (if (null? (cdr exp))
        #t
        (desugar `(if ,(cadr exp) ,(cons 'and (cddr exp)) #f))))
   
  ((pattern? '(or . _) exp)
    (if (null? (cdr exp))
        #f
        (desugar `(if ,(cadr exp) #t ,(cons 'or (cddr exp))))))
   
    ((pattern? '(quote _) exp)
     (quote-desugar (cadr exp)))
    ((pattern? '(quasiquote _) exp)
     (quasiquote-desugar (cadr exp)))
    ((pattern? '(unquote _) exp)
     (error "unquote"))
    
    ;;var
    ((symbol? exp) exp)
    ;; app
    ((pattern? `(,(disj symbol? list?) . _) exp)
     (if (equal? (car exp) 'list)
         (foldl (lambda (x ys)
                 (list 'cons (desugar x) ys))
               ''()
               (cdr exp))
         (map desugar exp)))
    (else exp)))

(define (quote-desugar term)
  (cond
   ((pair? term)
    `(cons ,(quote-desugar (car term))
           ,(quote-desugar (cdr term))))
   (else `(quote ,term))))

(define (quasiquote-desugar term)
  (cond
   ((pattern? '(unquote _) term)
    (desugar (cadr term)))
   ((pair? term)
    `(cons ,(quasiquote-desugar (car term))
           ,(quasiquote-desugar (cdr term))))
   (else (desugar `(quote ,term)))))



;; Recursion on sexp lambda terms

(define (primitive-value? expr)
  (or (null? expr)
      (symbol? expr)
      (number? expr)
      (string? expr)
      (boolean? expr)
      (pattern? '(quote _) expr)))

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
        (cdr existing)
        (begin (set-cell! free-vars (append frees (list (cons v `(vector-ref env ,len)))))
               `(vector-ref env ,len)))))

(define (closure-convert globally-bound bound free-vars e)
  (cond ((symbol? e)
         ;; var
         (if (member e bound)
             e
             (if (member e globally-bound)
                 (free! free-vars e)
                 (error "out of scope!" e))))
        ((primitive-value? e) e)
        ((pattern? '(if _ _ _) e)
         (let ((b (cadr e)) (thn (caddr e)) (els (cadddr e)))
           `(if ,(closure-convert globally-bound bound free-vars b)
                ,(closure-convert globally-bound bound free-vars thn)
                ,(closure-convert globally-bound bound free-vars els))))
        ((pattern? '(lambda _ _) e)
         (let ((vs (if (list? (cadr e)) (cadr e) (list (cadr e)))) (m (caddr e)))
           ;; lam
           ;; TODO trouble here [vs] with varargs
           (let ((inner-free-vars (make-cell '())))
             `(make-closure (lambda ,(cons 'env vs)
                              ,(closure-convert (append vs globally-bound) vs inner-free-vars m))
                            (vector . ,(map (lambda (a) (closure-convert globally-bound bound free-vars (car a)))
                                            (cell-value inner-free-vars)))))))
        ((list? e)
         ;; app
         `(invoke-closure . ,(map (lambda (a)
                                    (closure-convert globally-bound bound free-vars a)) e)))))


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

;; C

(define (concat lists)
  (if (null? lists)
      '()
      (append (car lists) (concat (cdr lists)))))

(define (up-to n)
  (cons 0 (iota '() (- n 1))))

(define (iota acc n)
  (if (zero? n)
      acc
      (iota (cons n acc) (- n 1))))

(define (c-gen def)
  (if (pattern? '(define _ (lambda (env . _) _)) def)
      (let ((name (cadr def)) (args (cdadr (caddr def))) (body (caddr (caddr def))))
        `(define-code ,name . ,(append (c-gen-pop-args '() args) (c-gen-body body))))
      (error "malformed definition")))

(define (c-gen-pop-args acc args)
  (if (null? args)
      acc
      (c-gen-pop-args (cons `(declare scm ,(car args) (pop)) acc) (cdr args))))

(define (c-gen-body body)
  (cond ((symbol? body) (list `(push ,body)))
        ((boolean? body) (list `(push ,(if body 'true 'false))))
        ((pattern? `(quote ,symbol?) body)
         (list `(push (make-symbol ,(symbol->string (cadr body))))))
        ((pattern? `(quote ,(disj null? (disj number? (disj boolean? string?)))) body)
         (list `(push ,(cadr body))))
        ((pattern? '(if _ _ _) body)
         (list `(if ,(cadr body)
                    ,(c-gen-body (caddr body))
                    ,(c-gen-body (cadddr body)))))
        ((pattern? '(vector-ref env _) body)
         (let ((i (caddr body)))
           (list `(push (ref (* env) ,i)))))
        ((pattern? '(make-closure _ (vector . _)) body)
         (let ((name (cadr body)) (env (cdr (caddr body))))
           (if (null? env)
               (list `(push (closure ,name 0 null)))
               (let ((env-heap (gensym name)))
                 (concat
                  (list (list `(declare scm** ,env-heap (gc-alloc* ,(length env))))
                        (map (lambda (i e)
                               (if (pattern? '(vector-ref env _) e)
                                   `(set! (ref ,env-heap ,i) ,e)
                                   `(set! (ref ,env-heap ,i) (gc-alloc-scm ,e))))
                             (up-to (length env)) env)
                        (list `(push (closure ,name ,(length env) ,env-heap)))))))))
        ((pattern? '(invoke-closure _ . _) body)
         (let ((continuation (cadr body))
               (arguments (cddr body)))
           (append (concat (map c-gen-body arguments))
                   (c-gen-body continuation))))
        (else (display (list 'c-gen-body body)) (newline)
              (error "error in c-gen-body"))))

;; Compiler

(define builtins '(cons car cdr null?))

(define (compile form)
  (let ((bound-variables (append '(halt) (append prims builtins))))
    
    (display 'compiling) (newline)
    (pretty-print form) (newline)
    (let ((cps-form (T-c form 'halt)))
      (display 'cps) (newline)
      (pretty-print cps-form) (newline)
      (let ((cc-form (closure-convert bound-variables bound-variables (make-cell '()) cps-form)))
        (display 'cc) (newline)
        (pretty-print cc-form) (newline)
        (let ((hoisted-form (hoist cc-form)))
          (display 'hoist) (newline)
          (for-each (lambda (i)
                      (pretty-print `(define ,(car i) ,(cdr i))))
                    lambdas) (newline)
          (pretty-print hoisted-form) (newline)
          (let ((c-codes (map (lambda (i) (c-gen `(define ,(car i) ,(cdr i)))) lambdas))
                (c-code-body (c-gen-body hoisted-form)))
            (display 'c-gen) (newline)
            (for-each (lambda (code) (display-code code) (newline)) c-codes) (newline)
            (pretty-print c-code-body) (newline)
            (let ((c-string ((formatter (~@ ~e ~%) ~e)
                             c-codes `(define-code scm-main . ,c-code-body))))
              (display 'emit-c) (newline)
              (display c-string)
              (newline)
              (with-output-to-file "moo.c"
                (lambda ()
                  (display c-string) (newline)))
              #t)))))))

(define (display-code code)
  (display "(") (display 'define-code) (display " ") (display (cadr code)) (newline)
  (for-each (lambda (inst) (display "  ") (pretty-print inst)) (cddr code))
  (display ")"))

(define (emit-c exp)
  (cond ((equal? exp 'null) ((formatter "NULL")))
        ((symbol? exp) ((formatter ~m) exp))
        ((number? exp) ((formatter ~a) exp))
        ((boolean? exp) ((formatter ~a) (if exp 'scm_true 'scm_false)))
        ((string? exp) ((formatter ~s) exp))
        ((pattern? '(* _) exp)
         ((formatter "*" ~e) (cadr exp)))
        ((pattern? '(define-code _ . _) exp)
         ((formatter "void " ~m "(scm **env) {" ~% (~@ ~e ~%) "}" ~%)
          (cadr exp) (cddr exp)))
        ((pattern? '(if _ _ _) exp)
         ((formatter "if (" ~e ") {" ~% (~@ ~e ~%) ~% " } else { " ~% (~@ ~e ~%) ~% " }" ~%)
          (cadr exp) (caddr exp) (cadddr exp)))
        ((pattern? '(pop) exp)
         ((formatter "stack_pop()")))
        ((pattern? '(push _) exp)
         ((formatter "stack_push(" ~e ");") (cadr exp)))
        ((pattern? '(closure _ _ _) exp)
         ((formatter "closure(" ~e ", " ~e ", " ~e ")") (cadr exp) (caddr exp) (cadddr exp)))
        ((pattern? '(vector-ref _ _) exp)
         ((formatter ~e "[" ~e "]") (cadr exp) (caddr exp)))
        ((pattern? '(ref _ _) exp)
         ((formatter ~e "[" ~e "]") (cadr exp) (caddr exp)))
        ((pattern? '(gc-alloc* _) exp)
         ((formatter "gc_alloc(" ~e "*sizeof(scm))") (cadr exp)))
        ((pattern? '(gc-alloc-scm _) exp)
         ((formatter "gc_alloc_scm(" ~e ")") (cadr exp)))
        ((pattern? '(make-symbol _) exp)
         ((formatter "symbol(" ~e ")") (cadr exp)))
        ((pattern? '(set! _ _) exp)
         ((formatter ~e " = " ~e ";") (cadr exp) (caddr exp)))
        ((pattern? '(declare _ _ _) exp)
         ((formatter ~a " " ~e " = " ~e ";") (cadr exp) (caddr exp) (cadddr exp)))
        (else (error "no emitter for: " exp))))

(define ~e (simple-formatter emit-c))


;;(compile '(lambda (x) (+ x x)))

(compile '(lambda (f x) (f (f (f x)))))

;;(compile '(lambda (b f x y) (if b (f x) (f y))))

;;(compile '(lambda (b f x y) (if (null? b) (f x) (f y))))

;;(compile (desugar ''(x y)))



;; (compile (desugar '((lambda (b f x y) (if b (f x) (f y)))
;;                     #t
;;                     (lambda (s) (s 'yoo 'zoo))
;;                     (lambda (p q) p)
;;                     (lambda (p q) q))))

;; (compile (desugar '(lambda (pattern? p e)
;;                      (cond ((null? p) (null? e))
;;                            ((equal? p '_) #t)
;;                            ((symbol? p) (and (symbol? e) (equal? p e)))
;;                            ((procedure? p) (p e))
;;                            (else (and (pair? e)
;;                                       (pattern? (car p) (car e))
;;                                       (pattern? (cdr p) (cdr e))))))))

(exit)

;; (compile (desugar '(define (desugar exp)
;;   (cond
;;    ((pattern? '(set! _ _) exp) `(set! ,(cadr exp) ,(desugar (caddr exp))))
 
;;    ((pattern? '(begin . _) exp)
;;     (cons 'begin (map desugar (cdr exp))))
   
;;    ((pattern? '(lambda _ . _) exp)
;;     (let ((params (cadr exp))
;;           (body (cddr exp)))
;;       `(lambda ,params ,(desugar (cons 'begin body)))))

;;    ((pattern? `(let ,symbol? ,list? . _) exp)
;;     (let* ((name (cadr exp))
;;            (bindings (caddr exp))
;; 	   (params (map car bindings))
;; 	   (values (map cadr bindings))
;; 	   (body (cdddr exp)))
;;       (desugar `((y-combinator (lambda (,name) (lambda ,params . ,body))) . ,values))))
   
;;    ((pattern? `(let ,list? . _) exp)
;;     (let* ((bindings (cadr exp))
;; 	   (params (map car bindings))
;; 	   (values (map cadr bindings))
;; 	   (body (cddr exp)))
;;       (desugar `((lambda ,params . ,body) . ,values))))
   
;;    ((pattern? `(let* ,list? . _) exp)
;;     (let* ((bindings (cadr exp))
;; 	   (body (cddr exp)))
;;       (desugar
;;        (append `(let (,(car bindings)))
;;                (if (null? (cdr bindings))
;;                    body
;;                    `((let* ,(cdr bindings) . ,body)))))))

;;    ((pattern? `(define _ _ . _) exp)
;;     (let ((name (caadr exp))
;; 	  (params (cdadr exp))
;; 	  (body (cddr exp)))
;;       `(define ,name ,(desugar `(lambda ,params . ,body)))))
   
;;    ((pattern? `(if _ _ _) exp)
;;     (let ((b (cadr exp))
;; 	  (then (caddr exp))
;; 	  (else (cadddr exp)))
;;       `(if ,(desugar b)
;;            ,(desugar then)
;;            ,(desugar else))))
   
;;    ((pattern? '(cond . _) exp)
;;     (cond
;;      ;; (cond)
;;      ((equal? '(cond) exp) '())
;;      ;; (cond (else b))
;;      ((and (equal? 'else (caadr exp))
;; 	   (= 2 (length exp)))
;;       (desugar (cons 'begin (cdadr exp))))
;;      ;; (cond (a b))
;;      ((= 2 (length exp))
;;       (desugar `(if ,(caadr exp)
;;                     ,(cons 'begin (cdadr exp))
;;                     '())))
;;      ;; (coond (a b) rest ...)
;;      ((> (length exp) 2)
;;       (desugar `(if ,(caadr exp)
;;                     ,(cons 'begin (cdadr exp))
;;                     (cond . ,(cddr exp)))))))
   
;;    ((pattern? '(and . _) exp)
;;     (if (null? (cdr exp))
;;         #t
;;         (desugar `(if ,(cadr exp) ,(cons 'and (cddr exp)) #f))))
   
;;   ((pattern? '(or . _) exp)
;;     (if (null? (cdr exp))
;;         #f
;;         (desugar `(if ,(cadr exp) #t ,(cons 'or (cddr exp))))))
   
;;     ((pattern? '(quote _) exp)
;;      (quote-desugar (cadr exp)))
;;     ((pattern? '(quasiquote _) exp)
;;      (quasiquote-desugar (cadr exp)))
;;     ((pattern? '(unquote _) exp)
;;      (error "unquote"))
    
;;     ;;var
;;     ((symbol? exp) exp)
;;     ;; app
;;     ((pattern? `(,(disj symbol? list?) . _) exp)
;;      (if (equal? (car exp) 'list)
;;          (fold (lambda (x ys)
;;                  (list 'cons (desugar x) ys))
;;                ''()
;;                (cdr exp))
;;          (map desugar exp)))
;;     (else exp))))))


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


