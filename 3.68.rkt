#lang sicp
(#%require (file "stream-utils.rkt"))

(define (louis-pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (louis-pairs (stream-cdr s) (stream-cdr t))))


#|
If we evaluate (louis-pairs integers integers), the evaluator tries to evaluate the arguments to interleave, which is a normal procedure, not a special form like cons-stream, but the second argument is a recursive call to louis-pairs. The evaluation of this argument will never terminate, and so interleave is never successfully called.
|#




;; tests
#|
(define iis (louis-pairs integers integers))
(stream-car iis)
(stream-take 100 (louis-pairs integers integers))
;|#
