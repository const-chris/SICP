#lang SICP

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))


(set-to-wow! z1)
"
       +---+---+
z1 --> | o | o |
       +-|-+-|-+
         |   |
         v   v
       +---+---+    +---+---+
x  --> | o | o ---> | o | / |
       +-|-+---+    +-|-+---+
         |            |
         v            v
       'wow          'b
"

(set-to-wow! z2)
"
       +---+---+    +---+---+    +---+---+
z2 --> | o | o ---> | o | o ---> | o | / |
       +-|-+---+    +-|-+---+    +-|-+---+
         |            |            |
         |            v            v
         |           'a           'b
         |                         ^
         |                         |
         |          +---+---+    +-|-+---+
         +--------> | o | o ---> | o | / |
                    +-|-+---+    +---+---+
                      |
                      v
                    'wow
"
