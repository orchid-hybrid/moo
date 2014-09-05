(load "../utilities/pattern.scm")

;; Recursion on sexp lambda terms
 
(define (traverse nil-case var-case quasiquote-case unquote-case lam-case app-case)
  (lambda (exp)
    (cond ((null? exp) nil-case)
          ((symbol? exp) (var-case exp))
          ((pattern? '(quasiquote _) exp) (quasiquote-case (cadr exp)))
          ((pattern? '(unquote _) exp) (unquote-case (cadr exp)))
          ((pattern? '(lambda _ _) exp)
           (let ((args (cadr exp)) (body (caddr exp)))
             (lam-case args body)))
          ((list? exp) (app-case exp))
          (else (error "not a thing: " exp)))))
