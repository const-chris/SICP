#lang sicp
(#%require (file "./data-directed-amb-evaluator.rkt"))
(#%provide (all-from (file "./data-directed-amb-evaluator.rkt"))
           (all-defined))

(define (require p) (if (not p) (amb)))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))


(input-definition '(define (require p) (if (not p) (amb))))

(input-definition '(define (an-integer-starting-from n)
                     (amb n (an-integer-starting-from (+ n 1)))))

(input-definition '(define (an-integer-between low high)
                     (require (<= low high))
                     (amb low (an-integer-between (+ low 1) high))))

(input-definition '(define (a-pythagorean-triple-between low high)
                     (let ((i (an-integer-between low high)))
                       (let ((j (an-integer-between i high)))
                         (let ((k (an-integer-between j high)))
                           (require (= (+ (* i i) (* j j)) (* k k)))
                           (list i j k))))))

#| (driver-loop) |#
