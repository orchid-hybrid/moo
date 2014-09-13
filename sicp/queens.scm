(define (accumulate f start more)
  (if (null? more)
      start
      (accumulate f (f start (car more)) (cdr more))))

(define (filter p l) (if (null? l) '() (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l)))))

(define (flatmap f l) (accumulate (lambda (start more) (append start (f more))) '() l))

(define (enumerate-interval start stop)
  (if (= start stop)
      (list start)
      (cons start (enumerate-interval (+ start 1) stop))))

(define (list-ref l idx)
  (if (= idx 0)
      (car l)
      (list-ref (cdr l) (- idx 1))))

(define (abs x) (if (< x 0) (- x) x))

;; stole from http://www.billthelizard.com/2011/06/sicp-242-243-n-queens-problem.html thanks bill

(define (make-position row col)
   (cons row col))

(define (position-row position)
   (car position))

(define (position-col position)
   (cdr position))

(define empty-board '())

(define (adjoin-position row col positions)
   (append positions (list (make-position row col))))
(define (safe? col positions)
   (let ((kth-queen (list-ref positions (- col 1)))
         (other-queens (filter (lambda (q)
                                 (not (= col (position-col q))))
                               positions)))
   (define (attacks? q1 q2)
     (or (= (position-row q1) (position-row q2))
         (= (abs (- (position-row q1) (position-row q2)))
            (abs (- (position-col q1) (position-col q2))))))

   (define (iter q board)
     (or (null? board)
         (and (not (attacks? q (car board)))
              (iter q (cdr board)))))
   (iter kth-queen other-queens)))

;;

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(display (queens 2))

