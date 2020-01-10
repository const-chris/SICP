#lang sicp
(newline)

; recursive:
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(newline)

; iterative:
(define (g n)
  (define (iter a b c count)
    (if (>= count n)
        c
        (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1)))) 
  (if (< n 3)
      n
      (iter 1 2 4 3)))

(g 3)
(g 4)
(g 5)
(g 6)
(g 7)
(g 10000)  ; runs fast
(newline)