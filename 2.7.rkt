#lang sicp
(newline)

(define (make-interval a b) (cons a b))




(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))




(define x (make-interval 2 9))
(display "x = ")
(display x)
(newline)

(display "(upper-bound x) = ")
(upper-bound x)
(display "(lower-bound x) = ")
(lower-bound x)




(newline)