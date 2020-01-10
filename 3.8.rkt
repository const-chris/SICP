#lang scheme

(define (make-f)
  (let ((x 1))
    (lambda (val)
        (set! x (* x val))
        x)))

(define f (make-f))
(define g (make-f))


;; tests
(newline)

#|
if the arguments to + are evaluated left-to-right,
(+ (f 0) (f 1)) = 0,
if the arguments are evaluated right-to-left,
(+ (f 0) (f 1)) = 1

the racket evaluator evaluates the arguments to + left-to-right
;|#

(display "(+ (f 0) (f 1)) = ")
(+ (f 0) (f 1))

(display "(+ (f 1) (f 0)) = ")
(+ (g 1) (g 0))




(newline)

