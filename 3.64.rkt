#lang sicp
(#%require (file "stream-utils.rkt"))
(#%provide stream-limit)


(define (stream-limit s tolerance)
  (let* ((tail (stream-cdr s))
         (e1 (stream-car s))
         (e2 (stream-car tail)))
    (if (< (abs (- e1 e2))
           tolerance)
      e2
      (stream-limit tail tolerance))))


;; test
#|
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x))
                  guesses)))
  guesses)

(stream-limit (sqrt-stream 2) 0.0001)
;|#
