#lang racket
(provide all)

(define (all pred? items)
  (cond ((null? items) true)
        ((not (pred? (car items))) false)
        (else
         (all pred? (cdr items)))))


#|
(define xs '(1 2 3 4 5 6))
(all (λ(x) (> x 0)) xs)
(all (λ(x) (> x 1)) xs)
;|#

