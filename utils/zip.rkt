#lang sicp
(#%require (file "any.rkt"))
(#%provide zip)

(define (zip . xss)
  (if (any null? xss)
      '()
      (cons (map car xss)
            (apply zip (map cdr xss)))))

;; (zip '(1 2 3) '(4 5 6) '(7 8 9 10) '(11 12 13))
