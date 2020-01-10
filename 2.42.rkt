#lang racket
(require "chapter2.rkt")
(newline)



(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (rest-of-queens)
                           (map (lambda (new-row)
                                  (adjoin-position new-row board-size rest-of-queens))
                                (enumerate-interval 1 board-size)))
                         (queen-cols (- k 1))))))
  (queen-cols board-size))




(define empty-board nil)




(define (safe? k positions)
  (define (find-queen col)
    (define (iter index squares)
      (if (= (car squares) 1)
          index
          (iter (+ index 1) (cdr squares))))
    (iter 1 col))
  
  (define (upper-diagonal kth-queen-row)
    (define (iter col row result)
      (if (or (< col 1) (< row 1))
          result
          (iter (- col 1) (- row 1) (cons (list-ref (list-ref positions (- col 1)) (- row 1)) result))))
    (iter k kth-queen-row nil))

  (define (lower-diagonal kth-queen-row)
    (define (iter col row result)
      (if (or (< col 1) (> row (length (car positions))))
          result
          (iter (- col 1) (+ row 1) (cons (list-ref (list-ref positions (- col 1)) (- row 1)) result))))
    (iter k kth-queen-row nil))

  (define (row kth-queen-row)
    (map (lambda (col) (list-ref col (- kth-queen-row 1)))
         positions))
          
  (let ((kth-queen-row (find-queen (list-ref positions (- k 1)))))
    (cond ((> (sum (row kth-queen-row)) 1) false)
          ((> (sum (upper-diagonal kth-queen-row)) 1) false)
          ((> (sum (lower-diagonal kth-queen-row)) 1) false)
          (else true))))




(define (adjoin-position new-row board-size rest-of-queens)
  (define (make-col row result)
    (cond ((< row 1) result)
          ((= row new-row) (make-col (- row 1) (cons 1 result)))
          (else (make-col (- row 1) (cons 0 result)))))
  (append rest-of-queens (list (make-col board-size nil))))




(queens 8)



  