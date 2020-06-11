#lang sicp
(#%provide any)

(define (any predicate items)
  (cond ((null? items) false)
        ((predicate (car items)) true)
        (else (any predicate (cdr items)))))

;; (any (lambda (x) (< x 0)) '(1 2 -3 4 0))