#lang sicp

(display "
a) estimating ϕ using recursive cont-frac:
")

(define (cont-frac n d k)
  (display (string-append "k = " (number->string k) ": "))
  (define (term i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i)
                    (term (+ i 1))))))
  (term 1))

(define (phi k)
  (/ 1 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  k)))

(phi 12)
(phi 13)




(display "
b) using iterative cont-frac:
")

(define (cont-frac-iter n d k)
  (display (string-append "k = " (number->string k) ": "))
  (define (term result i)
    (if (< i 1)
        result
        (term (/ (n i)
                 (+ (d i) result))
              (- i 1))))
  (term 0 k))

(define (phi-iter k)
  (/ 1 (cont-frac-iter (lambda (i) 1.0)
                       (lambda (i) 1.0)
                       k)))

(phi-iter 12)
(phi-iter 13)



(newline)
