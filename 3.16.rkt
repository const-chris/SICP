#lang SICP

;; Ben's procedure
(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

(define a '(1 2 3))
(count-pairs a)
#|
      +---+---+    +---+---+    +---+---+
a --> | o | o ---> | o | o ---> | o | / |
      +-|-+---+    +-|-+---+    +-|-+---+
        |            |            |
        v            v            v
        1            2            3
|#

(define b1 '(1))
(define b2 '(2))
(define b (cons b1 b2))
(set-cdr! b1 b2)
(count-pairs b)
#|
                     2
                     ^
                     |
      +---+---+    +-|-+---+
b --> | o | o ---> | o | / |
      +-|-+---+    +---+---+
        |            ^
        v            |
      +---+---+      |
      | o | o -------+
      +-|-+---+
        |
        v
        1
|#

(define c2 '(1))
(define c1 '(_ _))
(set-car! c1 c2)
(set-cdr! c1 c2)
(define c '(_ _))
(set-car! c c1)
(set-cdr! c c1)
(count-pairs c)
#|
                     +------------+
                     |            |
                     |            v
      +---+---+    +-|-+---+    +---+---+
c --> | o | o ---> | o | o ---> | o | / |
      +-|-+---+    +---+---+    +-|-+---+
        |            ^            |
        |            |            v
        +------------+            1
|#

(define d1 '(2 3))
(define d (cons 1 d1))
(set-cdr! d1 d)
;; never terminates:
;; (count-pairs d)
#|
        +-------------------------+
        |                         |
        v                         |
      +---+---+    +---+---+    +-|-+---+
d --> | o | o ---> | o | o ---> | o | / |
      +-|-+---+    +-|-+---+    +---+---+
        |            |
        v            v
        1            2
|#

