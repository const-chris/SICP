#lang sicp
(#%require (file "./4.35.rkt"))
(#%provide (all-from (file "./4.35.rkt"))
           (all-defined))

#|
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

Simply replacing an-integer-between with an-integer-starting-from in a-pythagorean-triple-between will not work,
because amb uses a depth-first search, which means when it encounters a failing value, it backtracks to the most
recent branch point and chooses a new path from there.

If we did replace an-integer-between with an-integer-starting-from in a-pythagorean-triple-between, when it
encountered a failure (or when we asked it to try-again) it would backtrack to the most recent non-exhausted
amb call, which would always be in the assignment to k. The sequence of values it tries, therefore, would be
a constant i, a constant j, and an infinitely increasing k.

Instead, we can write a-pythagorean-triple as:
|#

(define (a-pythagorean-triple)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


(input-definition '(define (a-pythagorean-triple)
                     (let ((k (an-integer-starting-from 1)))
                       (let ((j (an-integer-between 1 k)))
                         (let ((i (an-integer-between 1 j)))
                           (require (= (+ (* i i) (* j j)) (* k k)))
                           (list i j k))))))

#| (driver-loop) |#
