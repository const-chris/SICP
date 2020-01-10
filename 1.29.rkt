#lang sicp
(define (cube x) (* x x x))

(define (sum term a next b)
  ; (display (term a))
  ; (newline)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term x)
      (define (y k)
        (f (+ a (* k h))))
      (cond ((= x 0) (y x))
            ((= x n) (y x))
            ((= 0 (remainder x 2)) (* 2 (y x)))
            (else (* 4 (y x)))))
    (if (odd? n)
        "invalid argument: fourth argument to simpson must be an even integer"
        (* (/ h 3)
       (sum term 0 inc n)))))


(simpson cube 0 1 2)
(simpson cube 0 1 101)
(simpson cube 1 2 100)

; for the f(x) = x^3 from 0 to 1, simpson's rule produces the exact solution after just 3 terms (n = 2)