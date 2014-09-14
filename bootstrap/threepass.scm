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
    (let ((s (string->symbol (string-append "q" (number->string counter)))))
      (if (symbol-add s)
          s
          (loop (+ counter 1))))))

(define (set-remove-element set element)
  (if (null? set)
      set
      (if (equal? (car set) element)
          (set-remove-element (cdr set) element)
          (cons (car set) (set-remove-element (cdr set) element)))))
(define (set-remove set list)
  (if (null? list)
      set
      (set-remove (set-remove-element set (car list)) (cdr list))))

(define (set-intersect set-1 set-2)
  (if (null? set-1)
      set-1
      (if (member (car set-1) set-2)
          (cons (car set-1) (set-intersect (cdr set-1) set-2))
          (set-intersect (cdr set-1) set-2))))

(define (set-union set-1 set-2)
  (define (filter set-2)
    (if (null? set-2)
        set-2
        (if (member (car set-2) set-1)
            (filter (cdr set-2))
            (cons (car set-2) (filter (cdr set-2))))))
  (append set-1 (filter set-2)))

(define (set-union* sets)
  (if (null? sets)
      sets
      (set-union (car sets) (set-union* (cdr sets)))))


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
          (els (cadddr exp)))
      `(if ,(desugar b)
           ,(desugar then)
           ,(desugar els))))
   
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
         (foldr (lambda (x ys)
                  (list 'cons (desugar x) ys))
                ''()
                (cdr exp))
         (map desugar exp)))
    (else exp))) ;;(error "desugar" exp))))

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


(define (run form)
  (display form) (newline)
  (display 'desugar) (newline)
  (let ((desugared (desugar `(let () . ,form))))
    (display desugared) (newline)
    (display 'mutation-analysis) (newline)
    (let ((mut-form (mut-conv (make-cell '()) '() desugared)))
      (display mut-form) (newline)
      (display 'cps) (newline)
      (let ((cps-form (T-c mut-form 'halt)))
        (display cps-form)  (newline)))))

(
run '((define (caar x) (car (car x)))
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

(define (member elt l)
  (if (null? l) #f (if (equal? elt (car l)) #t (member elt (cdr l)))))


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
(define (display obj)
  (put-string (tostring obj)))
))
