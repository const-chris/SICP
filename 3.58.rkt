#lang sicp
(#%require (file "stream-utils.rkt"))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

#|
This procedure computes an unlimited-precision division of two numbers in a base designated by the radix argument.
The first element of the stream represents the whole-number part and the first fractional digit. The remaining digits each represent one additional digit of precision.
|#

(display "(expand 1 7 10) = ")
(stream-take 20 (expand 1 7 10))

(display "(expand 3 8 10) = ")
(stream-take 5 (expand 3 8 10))




;; tests
(display "(stream-take 20 (expand 3 2 10)) = ")
(stream-take 5 (expand 3 2 10))

(display "(stream-take 20 (expand 1 2 2)) = ")
(stream-take 5 (expand 1 2 2))

(display "(stream-take 20 (expand 1 4 2)) = ")
(stream-take 5 (expand 1 4 2))

(display "(stream-take 10 (expand 3 8 2)) = ")
(stream-take 5 (expand 3 8 2))
