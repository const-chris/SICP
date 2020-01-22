#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide partial-sums)

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s))))


;; test
#|
(stream-take 10 (partial-sums integers))
;|#
