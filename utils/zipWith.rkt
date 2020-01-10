#lang racket
(provide zipWith)

(define (zipWith proc L1 L2)
  (if (or (null? L1) (null? L2))
      '()
      (cons (proc (car L1) (car L2))
            (zipWith proc (cdr L1) (cdr L2)))))

#|
(define l1 '(1 2 3 4 5))
(define l2 '(1 2 3 4))
(define l3 '())

(zipWith + l1 l2)
(zipWith > l2 l1)
(zipWith * l1 l2)
(zipWith * l1 l3)
;|#
