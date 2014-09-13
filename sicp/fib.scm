(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(map display (map fib '(1 2 3 4 5 6 7 8 9)))


(define (fib* n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))


(map display (map fib* '(991 9992 9993 9994999 999599 969 979 989 9999)))
