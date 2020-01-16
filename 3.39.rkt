#lang racket
;; REVISIT -- test with serializer implementation

(define x 10)

(define s (make-serializer))

(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x))))))
 (s (lambda () (set! x (+ x 1)))))

#|
The possible outcomes are:

101: P1 sets x to 100 and then P2 increments x to 101.
  yes

121: P2 increments x to 11 and then P1 sets x to x * x.
  yes

110: P2 changes x from 10 to 11 between the two times that P1 accesses the value of
     x during the evaluation of (* x x).
  no: The two accesses to x in the squaring process are serialized with P2

11: P2 accesses x, then P1 sets x to 100, then P2 sets x.
  yes: The serialized part of P1 would have to happen first, creating a new value
       of 100, then the above happens. The important point here is that the setting
       of x by P1 is not serialized with P2.

100: P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.
  yes: The two serialized procedures run sequentially, but the outer P1 procedure,
       where the set! lives, is not serialized with P2.
|#