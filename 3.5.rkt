#lang sicp
(#%require (file "utils/square.rkt"))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))


(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (pred (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (monte-carlo trials experiment)
     (* (- x2 x1) (- y2 y1))))


(define (estimate-pi trials)
  (define (in-unit-circle? x y)
    (<= (+ (square x) (square y)) 1))
  (estimate-integral in-unit-circle? -1.0 1.0 -1.0 1.0 trials))




(newline)


(display "π ≈ ")
(estimate-pi (expt 10.0 6))


(newline)
