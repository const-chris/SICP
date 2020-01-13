#lang racket
;; REVISIT -- test with parallel-execute and make-serializer implementations

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))     ;; P1
                  (lambda () (set! x (* x x x))))  ;; P2

#|

10^6: P1 then P2 or
      P2 then P1

10^5: P1 runs between the first and second accesses in P2

10^4: P2 runs between the two accesses in P1 or
      P1 runs between the second and third accesses in P2

10^3: P2 creates a new value, then P1 runs, then P2 sets x
      
10^2: P1 creates a new value, then P2 runs, then P1 sets x

|#


(define y 10)
(define s (make-serializer))
(parallel-execute (s (lambda () (set! y (* y y))))     ;; P1
                  (s (lambda () (set! y (* y y y)))))  ;; P2

#|

10^6: P1 then P2 or
      P2 then P1
  yes

The rest are impossible, since P1 and P2 are serialized with each other.
Their processes cannot be interleaved.

|#