#lang sicp

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (filtered-accumulate filter combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filtered-accumulate filter combiner null-value term (next a) next b)))
        (else (filtered-accumulate filter combiner null-value term (next a) next b))))

(newline)
(display "a)")
(newline)

(define (prime? n)
  (define (smallest-divisor)
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor)))

(define (sum-squares-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))

(display "(sum-squares-primes 3 8): ")
(sum-squares-primes 3 8)


(newline)
(display "b)")
(newline)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (p i n)
  (define (relatively-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate relatively-prime? * 1 identity i inc (- n 1)))

(display "(p 1 6): ")
(p 1 6)