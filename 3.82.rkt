#lang sicp
(#%require (file "utils/square.rkt"))
(#%require (file "stream-utils.rkt"))




(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed))
      (monte-carlo
        (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))


(define (random-numbers-in-range low high)
  (let ((range (- high low)))
    (cons-stream
      (+ low (random range))
      (random-numbers-in-range low high))))




(define (estimate-integral f x1 x2 y1 y2)
  (monte-carlo
    (stream-map
      (lambda (r1 r2) (f r1 r2))
      (random-numbers-in-range x1 x2)
      (random-numbers-in-range y1 y2))
    0
    0))




;; test
;#|
(define pi
  (stream-map
    (lambda (p) (* 4.0 p))
    (estimate-integral
      (lambda (x y) (<= (+ (square x) (square y)) 1))
      0
      1.0
      0
      1.0)))


(display "π ≈ ")
(stream-ref pi 100000)
;|#
