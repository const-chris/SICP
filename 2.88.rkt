#lang racket
(require "generic-arithmetic-polynomials.rkt")
(newline)

#|
; inside install-scheme-number-package
(put 'negate '(scheme-number)
     (λ (x) (* x -1)))

; inside install-rational-package
(define (negate-rat x)
  (make-rational (* (numer x) -1) (denom x)))
(put 'negate '(rational) negate-rat)

;inside install-complex-package
(define (negate-complex x)
  (make-from-mag-ang (* (magnitude x) -1) (angle x)))
(put 'negate '(complex)
     (lambda (z) (tag (negate-complex z))))

; inside install-polynomial-package
(define (negate-poly p)
  (make-poly (variable p)
             (map negate-term (term-list p))))
(define (negate-term t)
  (make-term (order t) (negate (coeff t))))
(define (sub-poly x y)
  (add-poly x (negate-poly y)))
(put 'negate '(polynomial)
     (lambda (p) (tag (negate-poly p))))
(put 'sub '(polynomial polynomial)
     (lambda (p1 p2) (tag (sub-poly p1 p2))))

; generic negate
(define (negate x) (apply-generic 'negate x))
;|#



(define p1 (make-polynomial 'x
                            (list (make-term 2 2)
                                  (make-term 0 4))))

(define p2 (make-polynomial 'x
                            (list (make-term 2 1)
                                  (make-term 1 6))))

(display "p1          = ")
p1
(display "p2          = ")
p2
(display "(sub p1 p2) = ")
(sub p1 p2)




(newline)
