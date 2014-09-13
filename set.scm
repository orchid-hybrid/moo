;; This lets you do set operations on lists of symbols
;; It is mostly used for 

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
