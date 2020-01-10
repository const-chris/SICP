#lang racket
(require "generic-arithmetic-polynomials.rkt")
(provide install-generic-rational-package)

;; modified rational package
(define (install-generic-rational-package)
  ;; imported procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (=zero? x) (= (numer x) 0))
  (define (negate-rat x)
    (make-rat (* (numer x) -1) (denom x)))

  ;; updated procedures
  (define (make-rat n d) (cons n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))

  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put '=zero? '(rational) =zero?)
  (put 'negate '(rational) negate-rat)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)



;; tests
#|
(newline)
(install-generic-rational-package)
(newline)

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))
(display "rf          = ")
rf

(newline)

(display "(add rf rf) = ")
(add rf rf)

(display "simplified:   ")
(mul rf (make-rational (make-polynomial 'x '((0 2))) (make-polynomial 'x '((0 1)))))

(newline)
;|#
