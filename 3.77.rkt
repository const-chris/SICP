#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide integral)

(define (integral delayed-integrand initial-value dt)
  (cons-stream
    initial-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
          the-empty-stream
          (integral (delay (stream-cdr integrand))
                    (+ (* dt (stream-car integrand))
                       initial-value)
                    dt)))))


;; test
#|
(define (solve f y0 dt)
  (let ((y '*unassigned)
        (dy '*unassigned))
    (set! y (integral (delay dy) y0 dt))
    (set! dy (stream-map f y))
    y))

(stream-ref (solve (lambda (y) y)
                   1
                   0.0001)
            10000)
;|#
