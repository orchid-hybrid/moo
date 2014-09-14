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
