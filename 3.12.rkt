#lang sicp
(newline)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define x (list 'a 'b))
(display "x = ")
x

(define y (list 'c 'd))
(display "y = ")
y

#|
       ╔═══╦═══╗     ╔═══╦═══╗
x ═══> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║
         v             v
        'a            'b

       ╔═══╦═══╗     ╔═══╦═══╗
y ═══> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║
         v             v
        'c            'd
|#

(newline)




(define z (append x y))
(display "z = (append x y)\n  = ")
z

(display "(cdr x) = ")
(cdr x)
;; should be: '(b)

#|
       ╔═══╦═══╗     ╔═══╦═══╗
x ═══> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║
         v             v
        'a            'b

y ═══════════════════════════════════╗
                                     ║
                                     ║
                                     ║
                                     v
       ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗
z ═══> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║             ║             ║
         v             v             v             v
        'a            'b            'c            'd
|#

(newline)


(define w (append! x y))
(display "w = (append! x y)\n  = ")
w

(display "(cdr x) = ")
(cdr x)
;; shoule be: '(b c d)

#|
x ═══════╗                  y ═══════╗
         ║                           ║
         ║                           ║
         v                           v
       ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗
w ═══> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║             ║             ║
         v             v             v             v
        'a            'b            'c            'd
|#


(newline)