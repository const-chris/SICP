#lang sicp
(#%require (file "utils/rand-update.rkt"))




(define rand
  (let ((x 3))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset)
             (lambda (n)
               (set! x n)))
            (else (error "invalid argument -- RAND" m))))))




;; tests
(newline)

(define (rng n)
  (if (<= n 0)
      '()
      (cons (rand 'generate) (rng (- n 1)))))


(display "(rng 5): ")
(rng 5)

'reset
((rand 'reset) 3)

(display "(rng 5): ")
(rng 5)
(display "(rng 5): ")
(rng 5)

'reset
((rand 'reset) 3)

(display "(rng 10): ")
(rng 10)


;(rand 'fluffy)
(newline)
