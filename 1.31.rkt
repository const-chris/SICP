#lang sicp

(newline)
(display "a)")
(newline)

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(factorial 13)

(define (pi-product n)
  (* 4.0
     (product (lambda (x) (/ (if (odd? x)
                                 (+ x 1)
                                 (+ x 2))
                             (if (odd? x)
                                 (+ x 2)
                                 (+ x 1))))
              1
              inc
              n)))

(pi-product 10000)




(newline)
(display "b)")
(newline)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1))

(define (refactorial n)
  (product-iter identity 1 inc n))

(refactorial 13)
