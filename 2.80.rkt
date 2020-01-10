#lang racket
(require "generic-arithmetic.rkt")

(define (install-=zero?-package)
  (define (=zero-num? x) (= x 0))
  (define (=zero-rat? x) (= (numer x) 0))  
  (define (=zero-complex? x) (= (magnitude x) 0))
  (put '=zero? '(scheme-number) =zero-num?)
  (put '=zero? '(rational) =zero-rat?)
  (put '=zero? '(complex) =zero-complex?)
  'done)

(install-=zero?-package)


(define (=zero? x) (apply-generic '=zero? x))




(newline)


(define n1 (make-scheme-number 0))
(define n2 (make-scheme-number 4))

(=zero? n1)
(=zero? n2)
(newline)


(define r1 (make-rational 0 2.0))
(define r2 (make-rational 1 2.0))

(=zero? r1)
(=zero? r2)
(newline)


(define c1 (make-complex-from-mag-ang 0 1))
(define c2 (make-complex-from-real-imag 0 0))
(define c3 (make-complex-from-mag-ang 1 0))
(define c4 (make-complex-from-real-imag 0 1))

(=zero? c1)
(=zero? c2)
(=zero? c3)
(=zero? c4)
(newline)




