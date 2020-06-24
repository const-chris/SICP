#lang sicp
(#%provide (all-defined))

(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
#|
Under applicative-order evaluation, attempting to evaluate (factorial 5) will drop us into an infinite recursion.
The arguments to a procedure are evaluated before the procedure is applied, so (* 5 (factorial 4)) will be evaluated
before the call to unless. Indeed a call to (factorial n) will always try to evaluate (factorial (- n 1)) before
applying unless. This is obviously non-terminating.

Under normal-order evaluation, (factorial 5) produces the correct result. Procedure application happens before the
evaluation of the arguments, so

(unless (= n 1)
  (* n (factorial (- n 1)))
  1))

expands to

(if (= n 1)
    1
    (* n (factorial (- n 1))))

so (= n 1) is checked before (factorial (- n 1)) is evaluated, and if (= n 1) evaluates to true, (factorial (- n 1))
is never evaluated. So factorial always terminates.
|#


;#| tests
(define prog '(
  (define (unless condition usual-value exceptional-value)
    (if condition exceptional-value usual-value))

  (define (factorial n)
    (unless (= n 1)
      (* n (factorial (- n 1)))
      1))

  (factorial 5)))

#| runs forever
(#%require (file "./evaluator.rkt"))

(eval-sequence prog the-global-environment)
;|#

;#| works as expected
(#%require (file "./lazy-evaluator.rkt"))

(define (lazy-eval-sequence prog)
  (force-it (eval-sequence prog the-global-environment)))

(display "(factorial 5) = ")
(lazy-eval-sequence prog)
;|#
;|#
