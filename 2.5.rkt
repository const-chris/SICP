#lang sicp
(newline)

(define (divides? a b)
  (= (remainder b a) 0))




(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car p)
  (define (car-part x)
    (if (not (divides? 3 x))
        x
        (car-part (/ x 3))))
  (log (car-part p) 2))

(define (cdr p)
  (define (cdr-part x)
    (if (not (divides? 2 x))
        x
        (cdr-part (/ x 2))))
  (log (cdr-part p) 3))




(display "(car (cons 4 17)) = ")
(car (cons 4 17))

(display "(cdr (cons 10 3)) = ")
(cdr (cons 10 3))




; For this to be a valid representation of pairs of nonnegative integers, we need to show that given a pair (a b),
; the number 2^a3^b cannot be produced by any other pair of nonnegative integers.
; If this were not true then it would be possible to find some pair (c d) such that 2^a3^b = 2^c3^d.
; This would mean 2^a/2^c = 3^d/3^b
; and therefore 2^(a-c) = 3^(d-b)
; This could only be true if it were possible to factor 3^x as 2^y.
; This is patently false. 3 is prime, and so 3^x has only one prime factor, 3.

; By repeated division by either 2 or 3, we can isolate either the 2^a or the 3^b.
; From there we merely need to take the appropriate log (base 2 or base 3) in order to recover a or b.




(newline)