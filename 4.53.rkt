#lang sicp
(#%require (file "./4.39.rkt"))
(#%require (file "./4.51.rkt"))
(#%require (file "./4.52.rkt"))

(input-definition
  '(define (prime-sum-pair list1 list2)
     (let ((a (an-element-of list1))
           (b (an-element-of list2)))
       (require (prime? (+ a b)))
       (list a  b))))

(input-definition
  '(define (prime? n) (= n (smallest-divisor n))))

(input-definition
  '(define (smallest-divisor n) (find-divisor n 2)))

(input-definition
  '(define (find-divisor n test-divisor)
     (cond ((> (* test-divisor test-divisor) n) n)
           ((divides? test-divisor n) test-divisor)
           (else (find-divisor n (+ test-divisor 1))))))

(input-definition
  '(define (divides? a b) (= (remainder b a) 0)))

(driver-loop)

#|
evaluating:

(let ((pairs '()))
  (if-fail
    (let ((p (prime-sum-pair '(1 3 5 8)
                             '(20 35 110))))
      (permanent-set! pairs (cons p pairs))
      (amb))
    pairs))


results in:

    '((8 35) (3 110) (3 20))

as follows:

    - The algorithm searches for a prime-sum-pair to assign to p, and finds '(3 20).
    - It calls permanent-set! to change pairs to '((3 20)).
    - The (amb) call fails the current branch.
    - The algorithm backtracks to the latest branch point, which is in prime-sum-pair: (let ((b (an-element-of list2))).
    - p is assigned '(3 110).
    - pairs is permanent-set! to '((3 110) (3 20)).
    - (amb) fails this branch, and the algorithm backtracks to the same point as before.
    - There are no more elements of list2, so the algorithm backtracks one more branch point, still in prime-sum-pair: (let ((a (an-element-of list1))).
    - p is assigned '(8 35).
    - pairs is permanent-set! to '((8 35) (3 110) (3 20)).
    - (amb) causes backtracking to the previous branch point.
    - prime-sum-pair fails to find another pair.
    - The algorithm backtracks to if-fail.
    - pairs is returned.
|#

