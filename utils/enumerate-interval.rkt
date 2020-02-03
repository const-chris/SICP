#lang sicp
(#%provide enumerate-interval)

(define (enumerate-interval start end)
  (if (> start end)
      nil
      (cons start (enumerate-interval (+ start 1) end))))
