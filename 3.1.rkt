#lang racket
(newline)

(define (make-accumulator val)
  (lambda (n)
    (set! val (+ val n))
    val))

(define A1 (make-accumulator 0))
(define A2 (make-accumulator 10))

(display "(A1 0) = ")
(A1 0)
(display "(A1 8) = ")
(A1 8)

(display "(A2 0) = ")
(A2 0)
(display "(A2 5) = ")
(A2 5)

(display "(A2 2) = ")
(A1 2)




(newline)