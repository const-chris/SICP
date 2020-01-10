#lang racket
(newline)




(define (adjoin-set x set)
  (cond ((null? set) '())
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))


(display "(adjoin-set 5 '(3 4 6 7)) = ")
(adjoin-set 5 '(3 4 6 7))




(newline)