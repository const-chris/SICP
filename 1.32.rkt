#lang sicp

(newline)
(display "a)")
(newline)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (sum-integers a b)
  (sum identity a inc b))

(display "(sum-integers 1 10): ")
(sum-integers 1 10)

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (product identity 1 inc n))

(display "(factorial 13): ")
(factorial 13)




(newline)
(display "b)")
(newline)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (combiner result (term a)))))
  (iter a null-value))

(define (pi-sum n)
  (define (sum term a next b)
    (accumulate-iter + 0 term a next b))
  (* 8
     (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
          1
          (lambda (x) (+ x 4))
          n)))

(display "(pi-sum (expt 10 7)): ")
(pi-sum (expt 10 7))
