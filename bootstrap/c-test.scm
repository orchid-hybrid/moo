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


;; C

(define (concat lists)
  (if (null? lists)
      '()
      (append (car lists) (concat (cdr lists)))))

(define (up-to n)
  (cons 0 (iota '() (- n 1))))

(define (iota acc n)
  (if (= 0 n)
      acc
      (iota (cons n acc) (- n 1))))

(define (blah? x)
  (or (null? x)
      (number? x)
      (boolean? x)
      (string? x)))

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
        ((pattern? `(quote ,blah?) body)
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
        (else (error "error in c-gen-body"))))

(define (display-code code)
  (display "(") (display 'define-code) (display " ") (display (cadr code)) (newline)
  (for-each (lambda (inst) (display "  ") (display inst)) (cddr code))
  (display ")"))

(define codes '(
                (define lambda903
  (lambda (env k898 b f x y)
    (if b (invoke-closure f k898 x) (invoke-closure f k898 y))))
(define lambda904 (lambda (env k900 s) (invoke-closure s k900 'yoo 'zoo)))
(define lambda905 (lambda (env k901 p q) (invoke-closure k901 p)))
(define lambda906 (lambda (env k902 p q) (invoke-closure k902 q)))
(define lambda907
  (lambda (env k897)
    (invoke-closure
      (make-closure lambda903 (vector))
      k897
      #t
      (make-closure lambda904 (vector))
      (make-closure lambda905 (vector))
      (make-closure lambda906 (vector)))))

))

;(invoke-closure (make-closure lambda907 (vector)) (make-closure halt (vector)))

(for-each (lambda (e) (display (c-gen e)) (newline))
          codes)
