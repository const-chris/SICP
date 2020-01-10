#lang racket
(require "generic-arithmetic.rkt")
(provide raise
         make-real
         make-integer)


(define (make-real x) (cons 'real x))
(define (make-integer x) (cons 'integer x))

(define (install-raise-package)
  (define (raise-int x) (make-rational x 1))
  (define (raise-rat x) (make-real (/ (numer x) (denom x))))
  (define (raise-real x) (make-complex-from-real-imag x 0))
  (put 'raise '(integer) raise-int)
  (put 'raise '(rational) raise-rat)
  (put 'raise '(real) raise-real)
  'done)

(define (raise x)
  (if (get 'raise (list (type-tag x)))
      (apply-generic 'raise x)
      false))

(install-raise-package)
(newline)



#|
(display "(raise (make-integer 1))             = ")
(raise (make-integer 1))

(display "(raise (make-rational 1 2))          = ")
(raise (make-rational 1 2))

(display "(raise (make-real 0.434235))         = ")
(raise (make-real 0.434235))

(display "(raise (raise (make-real 0.434235))) = ")
(raise (raise (make-real 0.434235)))
;|#



(newline)
