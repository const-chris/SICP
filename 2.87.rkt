#lang racket
(require "generic-arithmetic-polynomials.rkt")
(require "utils/all.rkt")
(newline)


#|
; inside install-polynomial-package
(define (=zero? x)
  (all =zero? (map coeff (term-list x))))

(put '=zero? '(polynomial) =zero?)
;|#


(define p1 (make-polynomial 'x
                            (list (make-term 3 0)
                                  (make-term 1 0))))

(define p2 (make-polynomial 'x
                            (list (make-term 3 1)
                                  (make-term 1 0)
                                  (make-term 0 1))))


(display "p1          = ")
p1

(display "(=zero? p1) = ")
(=zero? p1)
(newline)


(display "p2          = ")
p2

(display "(=zero? p2) = ")
(=zero? p2)




(newline)