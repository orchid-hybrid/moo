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
      (char? expr)
      (pattern? '(quote _) expr)))

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
        ((char? e) e)
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
        ((char? body) (list `(push (make-char ,body))))
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
         (let ((name (cadr body)) (envi (cdr (caddr body))))
           (if (null? envi)
               (list `(push (closure ,name 0 null)))
               (let ((env-heap (gensym name)))
                 (concat
                  (list (list `(set! memory (gc-alloc*+* ,(length envi)))
                              `(declare scm** ,env-heap memory)
                              `(+= memory (* ,(length envi) (sizeof scm*))))
                        (concat (map (lambda (i e)
                                       (if (pattern? '(vector-ref env _) e)
                                           (list `(set! (ref ,env-heap ,i) ,e))
                                           (list `(set! (ref ,env-heap ,i) memory)
                                                 `(set! (* (ref ,env-heap ,i)) (* ,e))
                                                 `(+= memory (sizeof scm)))))
                                     (up-to (length envi)) envi))
                        (list `(push (closure ,name ,(length envi) ,env-heap)))))))))
        ((pattern? '(invoke-closure _ . _) body)
         (let ((continuation (cadr body))
               (arguments (cddr body)))
           (append (concat (map c-gen-body arguments))
                   (c-gen-body continuation))))
        (else (display (list 'c-gen-body body (pretty-print body))) (newline)
              (error "error in c-gen-body"))))

;; emit c

(define (emit-c exp)
  (cond ((equal? exp 'null) (null-fmt '()))
        ((equal? exp 'env) (env-fmt '()))
        ((symbol? exp) (sym-fmt (list exp)))
        ((char? exp) (char-fmt (list exp)))
        ((number? exp) (num-fmt (list exp)))
        ((null? exp) (empty-fmt '()))
        ((boolean? exp) (bool-fmt (list (if exp 1 0))))
        ((string? exp) (string-fmt (list exp)))
        ((pattern? '(* _) exp) (ptr-fmt (list (cadr exp))))
        ((pattern? '(* _ _) exp) (ptr2-fmt (list (cadr exp) (caddr exp))))
        ((pattern? '(define-code _ . _) exp) (define-code-fmt (list (cadr exp) (cddr exp))))
        ((pattern? '(if _ _ _) exp)
         (if-fmt (list (if (symbol? (cadr exp)) "scm_truepstar" "scm_truep")
                       (cadr exp) (caddr exp) (cadddr exp))))
        ((pattern? '(pop) exp) (pop-fmt '()))
        ((pattern? '(push _) exp) (push-fmt (list (cadr exp))))
        ((pattern? '(closure _ _ _) exp)
         (closure-fmt (list (cadr exp) (caddr exp) (cadddr exp))))
        ((pattern? '(vector-ref _ _) exp) (vref-fmt (list (cadr exp) (caddr exp))))
        ((pattern? '(ref _ _) exp)  (ref-fmt (list (cadr exp) (caddr exp))))
        ((pattern? '(sizeof _) exp) (sizeof-fmt (list (cadr exp))))
        ((pattern? '(gc-alloc* _) exp)   (gc-alloc*-fmt (list (cadr exp))))
        ((pattern? '(gc-alloc*+* _) exp) (gc-alloc*+*-fmt  (list (cadr exp) (cadr exp))))
        ((pattern? '(gc-alloc-scm _) exp) (gc-alloc-scm-fmt (list (cadr exp))))
        ((pattern? '(make-nil) exp) (make-nil-fmt '()))
        ((pattern? '(make-symbol _) exp) (make-symbol-fmt (list (cadr exp))))
        ((pattern? '(make-string _) exp) (make-string-fmt (list (cadr exp))))
        ((pattern? '(make-number _) exp) (make-number-fmt (list (cadr exp))))
        ((pattern? '(make-char _) exp) (make-char-fmt (list (cadr exp))))
        ((pattern? '(set! _ _) exp) (set!-fmt (list (cadr exp) (caddr exp))))
        ((pattern? '(+= _ _) exp)  (+=-fmt (list (cadr exp) (caddr exp))))
        ((pattern? '(declare _ _ (hold (pop))) exp)
         (declare-hold-fmt (list (cadr exp) (caddr exp) (cadr (cadddr exp)))))
        ((pattern? '(declare _ _ _) exp)
         (declare-fmt (list (cadr exp) (caddr exp) (cadddr exp))))
        (else (error "no emitter for: " exp))))


(define ~e (simple-formatter emit-c))

(define null-fmt (formatter (list "NULL")))
(define env-fmt (formatter (list "self->val.closure.environment")))
(define sym-fmt (formatter (list ~m)))
(define num-fmt (formatter (list ~a)))
(define char-fmt (lambda (c)
                   (cond ((equal? #\newline (car c)) "'\\n'")
                         ((equal? #\\ (car c)) "'\\\\'")
                         ((equal? #\' (car c)) "'\\''")
                         (else ((formatter (list "'" ~a "'")) c)))))
(define empty-fmt (formatter (list "(scm){ .typ=scm_type_null }")))
(define bool-fmt (formatter (list "bool(" ~a ")")))
(define string-fmt (formatter (list ~s)))
(define ptr-fmt (formatter (list "*" ~e)))
(define ptr2-fmt (formatter (list ~e "*" ~e)))
(define define-code-fmt
  (formatter (list "void " ~m "(scm *self) { void *memory; " ~% (~@ (list ~e ~%)) "}" ~%)))
(define if-fmt
  (formatter (list "if (" ~a "(" ~e ")) {"
                   ~% (~@ (list ~e ~%))
                   "} else {" ~%
                   (~@ (list ~e ~%)) "}" ~%)))
(define pop-fmt (formatter (list "stack_pop()")))
(define push-fmt (formatter (list "stack_push(" ~e ");")))
(define closure-fmt (formatter (list "closure(" ~e ", " ~e ", " ~e ")")))
(define vref-fmt (formatter (list ~e "[" ~e "]")))
(define ref-fmt (formatter (list ~e "[" ~e "]")))
(define sizeof-fmt (formatter (list "sizeof(" ~a ")")))
(define gc-alloc*-fmt (formatter (list "gc_alloc(" ~e "*sizeof(scm))")))
(define gc-alloc*+*-fmt
  (formatter (list "gc_alloc(" ~e "*sizeof(scm*) + " ~e "*sizeof(scm))")))
(define gc-alloc-scm-fmt (formatter (list "gc_alloc_scm(" ~e ")")))
(define make-nil-fmt (formatter (list "(scm){ .typ=scm_type_null }")))
(define make-symbol-fmt (formatter (list "sym(" ~e ")")))
(define make-string-fmt (formatter (list "str_alloc(" ~e ")")))
(define make-number-fmt (formatter (list "num(" ~e ")")))
(define make-char-fmt (formatter (list "(scm){ .typ=scm_type_char, .val.char_value=" ~e "}")))
(define set!-fmt (formatter (list ~e " = " ~e ";")))
(define +=-fmt (formatter (list ~e " += " ~e ";")))
(define declare-hold-fmt (formatter (list ~a " " ~e " = nursery_hold(" ~e ");")))
(define declare-fmt (formatter (list ~a " " ~e " = " ~e ";")))

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
                    (string->symbol . string_to_symbol)
                    (string->number . string_to_number)
                    (string-length . string_length)
                    (string-ref . string-ref)
                    (string-set! . string_set)
                    string-make
                    
                    peek-char read-char (eof-object? . eof_object_question)
                    
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
                                  (lambda ()
                                    ((formatter (list (~@ (list ~e ~%)) ~e))
                                     (list c-codes `(define-code scm-main . ,c-code-body)))))))
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


(define (last l)
  (if (null? l)
      #f
      (if (null? (cdr l))
          (car l)
          (last (cdr l)))))

(let ((debug #f))
  (let ((filename (or (last (command-line-arguments)) "test.scm")))
    (compile (append (if debug
                         '()
                         (append (scm-parse-file "prelude.scm")
                                (append (scm-parse-file "set.scm")
                                        (append (scm-parse-file "gensym.scm")
                                                (scm-parse-file "format-combinators.scm")
                                                ))))
                     (scm-parse-file filename))
             debug)))

(exit)
