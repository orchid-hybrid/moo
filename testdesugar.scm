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



(display (desugar '(let ()


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

)))
