#lang racket
(require "generic-arithmetic.rkt")

(define (install-equ?-package)
  (define (eq-rat? x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))
  (define (eq-complex? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (put 'equ? '(scheme-number scheme-number) =)
  (put 'equ? '(rational rational) eq-rat?)
  (put 'equ? '(complex complex) eq-complex?)
  'done)

(install-equ?-package)


(define (equ? a b) (apply-generic 'equ? a b))




(newline)


(define n1 (make-scheme-number 4))
(define n2 (make-scheme-number 4.0))
(define n3 (make-scheme-number 3))

(equ? n1 n2)
(equ? n1 n3)
(newline)


(define r1 (make-rational 2 4))
(define r2 (make-rational 1 2.0))
(define r3 (make-rational 1 3))

(equ? r1 r2)
(equ? r1 r3)
(newline)


(define c1 (make-complex-from-real-imag 1 0))
(define c2 (make-complex-from-mag-ang 1 0))
(define c3 (make-complex-from-real-imag 1 1))

(equ? c1 c2)
(equ? c1 c3)
(newline)




