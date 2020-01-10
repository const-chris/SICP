#lang sicp
(newline)

(define (divides? a b)
  (= 0 (remainder b a)))

(define (cont-frac n d k)
  (define (term result i)
    (if (< i 1)
        result
        (term (/ (n i)
                 (+ (d i) result))
              (- i 1))))
  (term 0 k))


(define (e k)
  (+ 2
     (cont-frac (lambda (i) 1.0)
                (lambda (i) (if (divides? 3 (+ i 1))
                                (/ (* 2 (+ i 1)) 3)
                                1))
                k)))



(display "e ≈ ")
(e 20)


(newline)