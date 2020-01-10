#lang sicp

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ result (term a)))))
  (iter a 0))


; tests:
(define (sum-integers a b)
  (sum identity a inc b))

(display "(sum-integers 1 10): ")
(sum-integers 1 10)

(define (sum-cubes a b)
  (sum (lambda (x) (* x x x)) a inc b))

(display "(sum-cubes 1 10): ")
(sum-cubes 1 10)

(define (pi-sum a b)
  (* 8
     (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
            a
            (lambda (x) (+ x 4))
            b)))

(display "pi = ")
(pi-sum 1 (expt 10 7))
