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

;; closure conversion



(define (primitive-value? expr)
  (or (null? expr)
      (symbol? expr)
      (number? expr)
      (string? expr)
      (boolean? expr)
      (pattern? '(quote _) expr)))





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
                    
                    string-append
                    (put-string . putstring)
                    newline
                    
                    (= . num_eq)
                    (< . lt) (> . gt)
                    (+ . add) (- . sub) (* . mul) (/ . divd) (modulo . modulo)
                    )))
(define (builtin? s) (assoc s builtins))


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

