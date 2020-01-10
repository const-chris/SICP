#lang sicp
(newline)

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))




(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (sq-list items)
  (map square items))




(define items (list 1 2 3 4))

(display "Implemented without map: (square-list items) = ")
(square-list items)

(display "Implemented with map: (square-list items) = ")
(sq-list items)




(newline)
