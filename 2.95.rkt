#lang racket
(require "generic-arithmetic-polynomials.rkt")
(require "2.94.rkt") ; gcd


(define p1 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(display "p1 = ")
p1

(define p2 (make-polynomial 'x '((2 11) (0 7))))
(display "p2 = ")
p2

(define p3 (make-polynomial 'x '((1 13) (0 5))))
(display "p3 = ")
p3

(newline)


(define q1 (mul p1 p2))
(display "q1 = (mul p1 p2) = ")
q1

(define q2 (mul p1 p3))
(display "q2 = (mul p1 p3) = ")
q2

(newline)


(display "(greatest-common-divisor q1 q2) = ")
(greatest-common-divisor q1 q2)