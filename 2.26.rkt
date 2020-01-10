#lang sicp
(newline)

(define x (list 1 2 3))
(define y (list 4 5 6))


(display "(append x y) = ")
(append x y)
; should print (1 2 3 4 5 6)

(display "(cons x y)   = ")
(cons x y)
; should print ((1 2 3) 4 5 6)

(display "(list x y)   = ")
(list x y)
; should print ((1 2 3) (4 5 6))




(newline)