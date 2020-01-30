#lang sicp
(#%require (file "stream-utils.rkt"))
(#%require (file "3.77.rkt"))


(define (solve-2nd a b dt y0 dy0)
  (let ((y '*unassigned)
        (dy '*unassigned)
        (ddy '*unassigned))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (integral (delay ddy) dy0 dt))
    (set! ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
    y))


;; test
(stream-take 20 (solve-2nd 1 1 0.001 0 1))
