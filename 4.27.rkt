#lang sicp
(#%require (file "./lazy-evaluator.rkt"))

#|
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;;; L-Eval input:
count

;;; L-Eval value:
1                                                 when we defined w, because our evaluator doesn't delay evaluation of definition-value,
                                                  the outer call to id is forced (as the operator part of an application), and count is
                                                  incremented by one.

;;; L-Eval input:
w

;;; L-Eval value:
10                                                as expected

;;; L-Eval input:
count

;;; L-Eval value:
2                                                 the outer call to id in the definition of w was forced and memoized at definition-time,
                                                  but the inner one isn't forced until we ask for the value of w. Because of memoization,
                                                  any further uses of w will not increment the count.
|#

(driver-loop)
