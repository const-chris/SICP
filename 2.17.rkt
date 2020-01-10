#lang sicp
(newline)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(display "(last-pair (list 23 72 149 34)) = ")
(last-pair (list 23 72 149 34))




(newline)