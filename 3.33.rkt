#lang sicp
(#%require (file "constraints.rkt"))

(define (averager x y avg)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder x y u)
    (multiplier avg v u)
    (constant 2 v)

    (probe "x" x)
    (probe "y" y)
    (probe "(avg x y)" avg)

    'ok))




;#| test
(define x (make-connector))
(define y (make-connector))
(define avg (make-connector))

(averager x y avg)

(set-value! x 1 'user)
(set-value! y 3 'user)

(forget-value! x 'user)
(set-value! avg 21 'user)
;|#
