#lang sicp
(newline)

(define π 3.141592653589793238)

(define (square x) (* x x))

(define (cont-frac n d k)
  (define (term result i)
    (if (< i 1)
        result
        (term (/ (n i)
                 (+ (d i) result))
              (- i 1))))
  (term 0 k))

(define (tan-cf θ k)
  (cont-frac (lambda (i) (if (= i 1)
                             θ
                             (- (square θ))))
             (lambda (i) (- (* 2 i) 1))
             k))


(display "tan(π/4) ≈ ")
(tan-cf (/ π 4) 9)

(newline)