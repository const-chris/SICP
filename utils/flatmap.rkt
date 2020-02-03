#lang sicp
(#%require (file "accumulate.rkt"))
(#%provide flatmap)

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
