#lang sicp
(#%require (file "./stream-utils.rkt"))
(#%require (file "./3.77.rkt"))

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
#|
If we use the scanning-out procedure from 4.16, solve becomes:
|#
(define (scanned-out-solve f y0 dt)
  (let ((y '*unassigned)
        (dy '*unassigned))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))
#|
and as we saw in 3.77, this works as expected.
If we use the following transformation, however,

(lambda ⟨vars⟩
  (let ((u '*unassigned*) (v '*unassigned*))
    (let ((a <e1>) (b <e2>))
      (set! u a)
      (set! v b))
    <e3>))

solve becomes:
|#
(define (temped-solve f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a (integral (delay dy) y0 dt))
          (b (stream-map f y)))
      (set! y a)
      (set! dy b)
      y)))

#|
This won't work.
When we construct the lambda from the internal let expression, Scheme eagerly evaluates its arguments,
and since stream-map immediately evaluates the stream-car, evaluating (stream-map f y) at this point
requires y to have a stream-car, but y is '*unassigned*, so an exception is thrown.
;|#

(stream-ref
  (temped-solve (lambda (y) y) 1 0.0001)
  10000)

