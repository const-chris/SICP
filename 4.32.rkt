#lang sicp
(#%require (file "./lazy-evaluator-with-streams.rkt"))

(define (run expr)
  (eval expr the-global-environment))

#|
The main difference between these lazy lists and the streams of chapter 3 is that even the heads of
these lists are lazy.

So we can do things like these:
|#

(run '(define xs (cons (/ 1 0) 1)))

(run '(display (cdr xs)))

(run '(newline))

(run '(define (length xs)
       (if (null? xs)
           0
           (+ 1 (length (cdr xs))))))

(run '(define ys (cons (/ 1 0) (cons 2 '()))))

(run '(length ys))

#|
(#%require (file "./stream-utils.rkt"))

With the chapter 3 streams, however, we run into trouble:

(define xs (cons-stream (/ 1 0) 1))   ;; blows up
(stream-cdr xs)                       ;; never even gets here
;|#
