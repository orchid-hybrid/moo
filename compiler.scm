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


;; Desugaring macros

(define (def! defsb d body)
  (let* ((defs (cell-value defsb))
         (existing (assoc d defs)))
    (if existing
        (error "identifier defined multiple times" d)
        (begin (set-cell! defsb (append defs (list (cons d body))))
               `(set! ,d (begin . ,body))))))

(define (desugar-body-with-defs ps)
  (let* ((defs (make-cell '()))
         (body (map (lambda (x)
                      (cond ((pattern? `(define ,list? . _) x)
                             (let ((name (caadr x))
                                   (params (cdadr x))
                                   (body (cddr x)))
                               (def! defs name (list `(lambda ,params . ,body)))))
                            ((pattern? `(define ,symbol? . _) x)
                             (let ((name (cadr x))
                                   (body (cddr x)))
                               (def! defs name body)))
                            (else x)))
                    ps))
         (declares (map (lambda (d) `(,(car d) '())) (cell-value defs))))
    (desugar `(let ,declares . ,body))))

(define (define? exp) (pattern? '(define . _) exp))

(define (desugar-body ps)
  (if (any define? ps)
      (desugar-body-with-defs ps)
     (desugar (cons 'begin ps))))

(define (desugar exp)
  (cond
   ((number? exp) exp)
   
   ((pattern? '(cons-stream _ _) exp)
    (desugar `(cons ,(cadr exp) (lambda () ,(caddr exp)))))
   
   ((pattern? '(set! _ _) exp) `(set! ,(cadr exp) ,(desugar (caddr exp))))
 
   ((pattern? '(begin . _) exp)
    (cons 'begin (map desugar (cdr exp))))
   
   ((pattern? '(lambda _ . _) exp)
    (let ((params (cadr exp))
          (body (cddr exp)))
      `(lambda ,params ,(desugar-body body))))

   ((pattern? `(letrec ,list? . _) exp)
    (let* ((bindings (cadr exp))
         (body (cddr exp))
         (defs (make-cell '()))
         (sets (map (lambda (b) (def! defs (car b) (cdr b))) bindings))
         (declares (map (lambda (d) `(,(car d) '())) (cell-value defs))))
    (desugar `(let ,declares . ,(append sets body)))))

   ((pattern? `(let ,symbol? ,list? . _) exp)
    (let* ((name (cadr exp))
	   (bindings (caddr exp))
           (params (map car bindings))
	   (values (map cadr bindings))
	   (body (cdddr exp)))
      (desugar `(letrec ((,name (lambda ,params . ,body)))
                  (,name . ,values)))))
   
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
     ;; (cond (els b))
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
         (foldr (lambda (x ys)
                  (list 'cons (desugar x) ys))
                ''()
                (cdr exp))
         (map desugar exp)))
    (else  exp))) ;;(error "desugar" exp))))

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

;; mutation boxing

(define (mutable! mutable-vars v)
  (let* ((mutables (cell-value mutable-vars)))
    (if (member v mutables)
        v
        (begin (set-cell! mutable-vars (cons v mutables))
               v))))

(define (mut-collect mvars e)
  (cond ((pattern? '(begin . _) e)
         `(begin . ,(map (lambda (se) (mut-collect mvars se)) (cdr e))))
        ((pattern? '(if _ _ _) e)
         (let ((b (cadr e)) (thn (caddr e)) (els (cadddr e)))
           (mut-collect mvars b)
           (mut-collect mvars thn)
           (mut-collect mvars els)))
        ((pattern? '(lambda _ _) e)
         (let ((vs (if (list? (cadr e)) (cadr e) (list (cadr e))))
               (m (caddr e)))
           (let ((inner-mut-vars (make-cell '())))
             (mut-collect inner-mut-vars  m)
             ;; dont bubble up vars bound by this lambda
             (map (lambda (v) (if (member v vs)
                                   '()
                                  (mutable! mvars v))) (cell-value inner-mut-vars)))))
        ((pattern? '(set! _ _) e)
         (mutable! mvars (cadr e))
         (mut-collect mvars (caddr e)))
        ((pattern? '(_ . _) e)
         (mut-collect mvars (car e))
         (map (lambda (a) (mut-collect mvars a)) (cdr e)))
        (else '()))
  e)

(define (mut-conv mvars replace e)
  (cond ((symbol? e) (if (member e replace) `(car ,e) e))
        ((primitive-value? e) e)
        ((pattern? '(begin . _) e)
         `(begin . ,(map (lambda (se) (mut-conv mvars replace se)) (cdr e))))
        ((pattern? '(if _ _ _) e)
         (mut-collect mvars e)
         (let ((b (cadr e)) (thn (caddr e)) (els (cadddr e)))
           `(if ,(mut-conv mvars replace b)
                ,(mut-conv mvars replace thn)
                ,(mut-conv mvars replace els))))
        
        ((pattern? '(lambda _ _) e)
         (let* ((vs (cadr e))
                (vs-normal (if (list? vs) vs (list vs)))
                (m (caddr e)))
           (let* ((inner-mut-vars (make-cell '()))
                  (_ (mut-collect inner-mut-vars  m))
                  (not-shadowed (set-remove replace vs-normal))
                  ;; dont replace vars shadowed by this lambda that aren't mutable
                  (conv-body (mut-conv inner-mut-vars (append not-shadowed
                                                              (cell-value inner-mut-vars)) m))
                  (boxed-vars (map (lambda (v) `(cons ,v '()))
                                   (set-intersect vs-normal (cell-value inner-mut-vars)))))
             (if (equal? '() boxed-vars)
                 `(lambda ,vs ,conv-body)
                 `(lambda ,vs
                    ((lambda ,vs ,conv-body) . ,boxed-vars))))))
        ((pattern? '(set! _ _) e)
         (mut-collect mvars e)
         `(set-car! ,(cadr e) ,(mut-conv mvars replace (caddr e))))
        ((pattern? '(_ . _) e)
         (mut-collect mvars e)
         `(,(mut-conv mvars replace (car e)) . ,(map (lambda (a) (mut-conv mvars replace a)) (cdr e))))
        (else (error "dont know how to mut-conv" e))))


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
                                        ; We have to reify the cont to avoid
                                        ; a possible code blow-up:
         (define $rv (gensym '$rv))
         (define cont `(lambda(,$rv) ,(k $rv)))
         (T-c expr cont))

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

             (define (if-form v)
               (T-k bool (lambda (aexp)
                             `(if ,aexp 
                                  ,(T-c thn v)
                                  ,(T-c els v)))))
             (if (symbol? c)
                 (if-form c)
                 (let (($k (gensym '$k)))
                   `((lambda (,$k) ,(if-form $k)) ,c))))))

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
             (if (builtin? e)
                 `(make-closure ,(cdr (assoc e builtins)) (vector))
                 (if (member e globally-bound)
                     (free! free-vars e)
                     (error "out of scope!" e)))))
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
      (c-gen-pop-args (cons `(declare scm* ,(car args) (hold (pop))) acc) (cdr args))))

(define (c-gen-body body)
  (cond ((null? body) (list `(push (make-nil))))
        ((symbol? body) (list `(push (* ,body))))
        ((number? body) (list `(push (make-number ,body))))
        ((boolean? body) (list `(push ,body)))
        ((string? body) (list `(push (make-string ,body))))
        ((pattern? `(quote ,symbol?) body)
         (list `(push (make-symbol ,(symbol->string (cadr body))))))
        ((pattern? `(quote ,(disj null? (disj number? (disj boolean? string?)))) body)
         (c-gen-body (cadr body)))
        ((pattern? '(if _ _ _) body)
         (list `(if ,(if (pattern? '(vector-ref env _) (cadr body))
                         `(* ,(cadr body))
                         (cadr body))
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
                  (list (list `(set! memory (gc-alloc*+* ,(length env)))
                              `(declare scm** ,env-heap memory)
                              `(+= memory (* ,(length env) (sizeof scm*))))
                        (concat (map (lambda (i e)
                                       (if (pattern? '(vector-ref env _) e)
                                           (list `(set! (ref ,env-heap ,i) ,e))
                                           (list `(set! (ref ,env-heap ,i) memory)
                                                 `(set! (* (ref ,env-heap ,i)) (* ,e))
                                                 `(+= memory (sizeof scm)))))
                                     (up-to (length env)) env))
                        (list `(push (closure ,name ,(length env) ,env-heap)))))))))
        ((pattern? '(invoke-closure _ . _) body)
         (let ((continuation (cadr body))
               (arguments (cddr body)))
           (append (concat (map c-gen-body arguments))
                   (c-gen-body continuation))))
        (else (display (list 'c-gen-body body (pretty-print body))) (newline)
              (error "error in c-gen-body"))))

;; emit c


(define (emit-c exp)
  (cond ((equal? exp 'null) ((formatter "NULL")))
        ((equal? exp 'env) ((formatter "self->val.closure.environment")))
        ((symbol? exp) ((formatter ~m) exp))
        ((number? exp) ((formatter ~a) exp))
        ((null? exp) ((formatter "(scm){ .typ=scm_type_null }")))
        ((boolean? exp) ((formatter "bool(" ~a ")") (if exp 1 0)))
        ((string? exp) ((formatter ~s) exp))
        ((pattern? '(* _) exp)
         ((formatter "*" ~e) (cadr exp)))
        ((pattern? '(* _ _) exp)
         ((formatter ~e "*" ~e) (cadr exp) (caddr exp)))
        ((pattern? '(define-code _ . _) exp)
         ((formatter "void " ~m "(scm *self) { void *memory; " ~% (~@ ~e ~%) "}" ~%)
          (cadr exp) (cddr exp))
         
         ;; ((formatter "void " ~m "(scm **env) { void *memory; puts(\"" ~m "\");" ~% (~@ ~e ~%) "}" ~%)
         ;;  (cadr exp) (cadr exp) (cddr exp))
         
         )
        ((pattern? '(if _ _ _) exp)
         ((formatter "if (" ~a "(" ~e ")) {" ~% (~@ ~e ~%) "} else {" ~% (~@ ~e ~%) "}" ~%)
          (if (symbol? (cadr exp)) "scm_truepstar" "scm_truep")
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
        ((pattern? '(sizeof _) exp)
         ((formatter "sizeof(" ~a ")") (cadr exp)))
        ((pattern? '(gc-alloc* _) exp)
         ((formatter "gc_alloc(" ~e "*sizeof(scm))") (cadr exp)))
        ((pattern? '(gc-alloc*+* _) exp)
         ((formatter "gc_alloc(" ~e "*sizeof(scm*) + " ~e "*sizeof(scm))") (cadr exp) (cadr exp)))
        ((pattern? '(gc-alloc-scm _) exp)
         ((formatter "gc_alloc_scm(" ~e ")") (cadr exp)))
        ((pattern? '(make-nil) exp)
         ((formatter "(scm){ .typ=scm_type_null }")))
        ((pattern? '(make-symbol _) exp)
         ((formatter "sym(" ~e ")") (cadr exp)))
        ((pattern? '(make-string _) exp)
         ((formatter "str_alloc(" ~e ")") (cadr exp)))
        ((pattern? '(make-number _) exp)
         ((formatter "num(" ~e ")") (cadr exp)))
        ((pattern? '(set! _ _) exp)
         ((formatter ~e " = " ~e ";") (cadr exp) (caddr exp)))
        ((pattern? '(+= _ _) exp)
         ((formatter ~e " += " ~e ";") (cadr exp) (caddr exp)))
        ((pattern? '(declare _ _ (hold (pop))) exp)
         ((formatter ~a " " ~e " = nursery_hold(" ~e ");") (cadr exp) (caddr exp) (cadr (cadddr exp))))
        ((pattern? '(declare _ _ _) exp)
         ((formatter ~a " " ~e " = " ~e ";") (cadr exp) (caddr exp) (cadddr exp)))
        (else (error "no emitter for: " exp))))

(define ~e (simple-formatter emit-c))

;; Compiler

(define (prepare-builtins l)
  (map (lambda (b)
         (if (pair? b) b (cons b b))) l))

(define builtins (prepare-builtins
                  '(halt
                    (exit . scm_exit)

                    (random . scm_random)
                    
                    null? pair? char? string? boolean?
                    number? procedure? symbol?
                    
                    eq?
                    
                    cons car cdr
                    (set-car! . set_car) (set-cdr! . set_cdr)
                    
                    (char->string . char_to_string)
                    (number->string . number_to_string)
                    (symbol->string . symbol_to_string)
                    
                    string-append
                    (put-string . putstring)
                    newline
                    
                    (= . num_eq)
                    (< . lt) (> . gt)
                    (+ . add) (- . sub) (* . mul) (/ . divd) (modulo . modulo)
                    )))

(define (builtin? s) (assoc s builtins))

(define (compile form debug)
  (let ((form `(let () . ,form))
        (bound-variables '()))
    (display 'compiling) (newline)
    (when debug (pretty-print form) (newline))
    (let ((desugared (desugar form)))
      (display 'desugaring) (newline)
      (when debug (pretty-print desugared) (newline))
      (let ((mut-form (mut-conv (make-cell '()) '() desugared)))
        (display 'mutation-analysis) (newline)
        (when debug (pretty-print mut-form) (newline))
        (let ((cps-form (T-c mut-form 'halt)))
          (display 'cps) (newline)
          (when debug (pretty-print cps-form) (newline))
          (let ((cc-form (closure-convert bound-variables bound-variables (make-cell '()) cps-form)))
            (display 'cc) (newline)
            (when debug (pretty-print cc-form) (newline))
            (let ((hoisted-form (hoist cc-form)))
              (display 'hoist) (newline)
              (when debug 
                    (for-each (lambda (i)
                                (pretty-print `(define ,(car i) ,(cdr i))))
                              lambdas) (newline)
                              (pretty-print hoisted-form) (newline))
                        (let ((c-codes (map (lambda (i) (c-gen `(define ,(car i) ,(cdr i)))) lambdas))
                              (c-code-body (c-gen-body hoisted-form)))
                          (display 'c-gen) (newline)
                          (when debug (for-each (lambda (code) (display-code code) (newline)) c-codes) (newline)
                                (pretty-print c-code-body) (newline))
                          (let ((c-string (with-output-to-string
                                            (lambda () ((formatter (~@ ~e ~%) ~e)
                                                        c-codes `(define-code scm-main . ,c-code-body))))))
                            (display 'emit-c) (newline)
                            (when debug (display c-string)
                                  (newline))
                            (with-output-to-file "moo.c"
                              (lambda ()
                                (display c-string) (newline)))
                            #t)))))))))

(define (display-code code)
  (display "(") (display 'define-code) (display " ") (display (cadr code)) (newline)
  (for-each (lambda (inst) (display "  ") (pretty-print inst)) (cddr code))
  (display ")"))


;;(compile '(lambda (x) (+ x x)))

;;(compile '(lambda (f x) (f (f (f x)))))

;;(compile '(lambda (b f x y) (if b (f x) (f y))))

;;(compile '(lambda (b f x y) (if (null? b) (f x) (f y))))

;;(compile (desugar ''(x y)))

;; (compile (desugar '((lambda (b f x y)  (if b (f x) (f y)))
;;                     #t
;;                     (lambda (s) (s 'yoo 'zoo))
;;                     (lambda (p q) p)
;;                     (lambda (p q) q))))

;;(compile (desugar '(car '(x y))))

;; (compile (desugar '(((lambda (s)
;;                        (lambda (l)
;;                          (if (null? l) (display 'end)
;;                              (begin (display (car l)) (s (cdr l))))))
;;                      ((lambda (s)
;;                         (lambda (l)
;;                           (if (null? l) (display 'end)
;;                               (begin (display (car l)) (s (cdr l))))))
;;                       ((lambda (s)
;;                         (lambda (l)
;;                           (if (null? l) (display 'end)
;;                               (begin (display (car l)) (s (cdr l))))))
;;                       (lambda (l)
;;                         (if (null? l) (display 'end)
;;                             (begin (display (car l)) (display 'no)))))))
;;                     '(foo bar baz))))

;; (compile (desugar '(((lambda (f)
;;                        (f (f (f (lambda (e) (display 'no))))))
;;                      (lambda (s)
;;                        (lambda (l)
;;                          (if (null? l) (display 'end)
;;                              (begin (display (car l)) (s (cdr l)))))))
;;                     '(foo bar baz))))

;; (compile (desugar '(((lambda (r)
;;                        ((lambda (f) (f f))
;;                         (lambda (f) (r (lambda (x) ((f f) x))))))
;;                      (lambda (s)
;;                        (lambda (l)
;;                          (if (null? l) (display 'end)
;;                              (begin (display (car l)) (s (cdr l)))))))
;;                     '(foo bar baz quux a b c d e f g h i z e e e e o o u o)))

;; (compile (desugar '(display (((lambda (r)
;;                                 ((lambda (f) (f f))
;;                                  (lambda (f) (r (lambda (x) ((f f) x))))))
;;                               (lambda (f)
;;                                 (lambda (a b count)
;;                                   (if (< count 0)
;;                                       b
;;                                       (f (+ a b) a (- count 1))))))
;;                              1 0 50))))

;; (compile (desugar '((lambda (a)
;;                                (display a)
;;                                (set! a 2)
;;                                (display a)
;;                                (set! a 3)
;;                                (display a)) 1)))

;; (compile (desugar '(let ((x (cons 'a 'b)))
;;                      (display (car x))
;;                      (set-car! x 'e)
;;                      (display (car x))
;;                      (display (cdr x)))))

;; (compile (desugar '((lambda (u) (u u))
;;                     (lambda (f) (display 'ok) (f f)))))

;; (compile (desugar '((lambda (s) ((lambda (u) (u u))
;;                                  (lambda (f) (display s) (f f))))
;;                     'not-okay)))


;; (compile '((define p
;;               (lambda (x)
;;                 (+ 1 (q (- x 1)))))
            
;;             (define q
;;               (lambda (y)
;;                 (if (= 0 y)
;;                     0
;;                     (+ 1 (p (- y 1))))))
            
;;             (define x (p 5))
            
;;             (define y x)
            
;;             y))

(define (last l)
  (if (null? l)
      #f
      (if (null? (cdr l))
          (car l)
          (last (cdr l)))))

(let ((filename (or (last (command-line-arguments)) "test.scm")))
  (compile (append (scm-parse-file "prelude.scm")
                   (scm-parse-file filename))
           #f))

(exit)

;; testing

(define (cps f)
  (lambda args
    ((car args) (apply f (cdr args)))))

(define (make-closure code env) (cons code env))
(define (invoke-closure closure . args) (apply (car closure) (cons (cdr closure) args)))

(define *k (make-closure (lambda (k env x y) (invoke-closure k (* x y))) (vector)))
(define +k (make-closure (lambda (k env x y) (invoke-closure k (+ x y))) (vector)))
(define halt (make-closure (lambda (r env) (display r)) (vector)))


