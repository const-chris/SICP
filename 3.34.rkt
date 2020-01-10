#lang racket
(require "constraints.rkt")

;; Louis's squarer
(define (squarer a b)
  (multiplier a a b))

#|
The problem with this idea is that multiplier is not equipped to contrain its multiplicands
as square roots of the product. Setting b will not determine a.
|#

;#| test
(define a (make-connector))
(define b (make-connector))
(squarer a b)

(probe "a" a)
(probe "b" b)

(set-value! a 2 'user)
(forget-value! a 'user)
; this half works

(set-value! b 4 'user)
; but this half is borked
;|#