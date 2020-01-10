#lang sicp
(newline)

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

#|
mystery destructively reverses a list
|#


(define v (list 'a 'b 'c 'd))
(display "v = ")
v
#|
       ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗
v ═══> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║             ║             ║
         v             v             v             v
        'a            'b            'c            'd
|#


(define w (mystery v))
#|
v ═════════════════════════════════════════════════╗
                                                   ║
                                                   ║
                                                   v
       ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗     ╔═══╦═══╗
w ═══> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ o═════> ║ o ║ / ║
       ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝     ╚═║═╩═══╝
         ║             ║             ║             ║
         v             v             v             v
        'd            'c            'b            'a
|#


(display "w = (mystery v)\n  = ")
w

(display "v = ")
v




(newline)