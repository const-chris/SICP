#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.77.rkt"))
(#%require (file "3.78.rkt"))

(define (solve-2nd f dt y0 dy0)
  (let ((y '*unassigned)
        (dy '*unassigned)
        (ddy '*unassigned))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (stream-map f dy y))
    y))


;; test
(stream-take 20 (solve-2nd + 0.001 0 1))
