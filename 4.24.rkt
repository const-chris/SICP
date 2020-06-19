#lang sicp
(#%require (file "./utils/repeat.rkt"))
(#%require (file "./utils/accumulate.rkt"))
(#%require (file "./evaluator-2.rkt"))
#|
Unexpected behavior: different results depending on whether it's run directly with the racket command or in the REPL.
REPL results are more in line with what I'd expect (~2x speedup).
Running with the racket command gives more variable results, and prog2 usually runs quite a bit slower with the refined evaluator.
Seems that Racket is doing some extra compilation when creating a module.
(Similar weirdness happens with submodules within a file.)
|#

(define REPS 100.0)

(define (average-runs proc prog)
  (let ((runs (map proc (repeat REPS prog))))
    (/ (accumulate + 0 runs) REPS)))

(define (test prog)
  (let ((original (average-runs run prog))
        (refined  (average-runs run-2 prog)))
    (newline)
    (display "program:            ")
    (display prog)
    (newline)
    (display "original evaluator: ")
    (display original)
    (newline)
    (display "refined evaluator:  ")
    (display refined)
    (newline)
    (display "speed-up factor:    ")
    (display (/ original (* 1.0 refined)))
    (newline)))

(define prog1
  '(
    (define (loop x)
      (if (= 0 x)
          'ok
          (loop (- x 1))))

    (loop 100000)))

(define prog2
  '(
    ((lambda ()
       (define (factorial n)
         (if (= n 0)
             1
             (* n (factorial (- n 1)))))

       (factorial 20)))))

(define prog3
  '(
    (define (odd? x)
      (if (= 0 x)
          'false
          (even? (- x 1))))

    (define (even? x)
      (if (= 0 x)
          'true
          (odd? (- x 1))))

    (even? 135234)))

(for-each
  test
  (list prog1
        prog2
        prog3))
