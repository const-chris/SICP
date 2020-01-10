#lang racket
(newline)

(define (apply-generic op arg) (arg op))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))




(define (make-from-mag-ang x y)
  (lambda (op)
    (cond ((eq? op 'real-part) (* x (cos y)))
          ((eq? op 'imag-part) (* x (sin y)))
          ((eq? op 'magnitude) x)
          ((eq? op 'angle) y)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op)))))



(define z (make-from-mag-ang 1 (/ pi 4)))

(display "(real-part z) = ") 
(real-part z)

(display "(imag-part z) = ")
(imag-part z)

(display "(magnitude z) = ")
(magnitude z)

(display "(angle z)     = ")
(angle z)




(newline)