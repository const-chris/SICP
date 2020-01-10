#lang racket
(require "chapter2.rkt")
(provide queens empty-board adjoin-position safe?)
(newline)


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row k rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))




(define empty-board nil)


(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))


(define (safe? k positions)
  (let ((last-queen (car positions))
        (rest-of-queens (cdr positions)))   
    (define (make-diagonal row-op)
      (define (iter row result)
        (if (= (length result) k)
            result
            (iter (row-op row) (append result (list row)))))
      (iter last-queen nil))
    (let ((upper-diagonal (make-diagonal dec))
          (lower-diagonal (make-diagonal inc)))         
      (cond ((some (lambda (x) (= x last-queen)) rest-of-queens) false)
            ((some identity
                   (map (lambda (x y z) (or (= x y) (= x z)))
                        rest-of-queens
                        (cdr upper-diagonal)
                        (cdr lower-diagonal)))              
             false)
            (else true)))))




(queens 8)




(newline)


