#lang sicp
(newline)

(define (sum . xs)
  (if (null? xs)
      0
      (+ (car xs) (apply sum (cdr xs)))))


(define (average . xs)
  (/ (apply sum xs) (length xs)))


(define (compose . fs)
  (if (null? fs)
      identity
      (lambda (x) ((car fs) ((apply compose (cdr fs)) x)))))


(define (repeated f n)
  (define (iter result count)
    (if (<= count 0)
        result
        (iter (compose f result) (- count 1)))) 
  (iter identity n))


(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (let ((tolerance 0.000001))
      (< (abs (- a b)) tolerance)))
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))


(define (average-damp f)
  (lambda (x) (average x (f x))))


(define (nth-root n x)
  (let ((times (floor (log n 2)))
        (fn (lambda (y) (/ x (expt y (- n 1))))))
    (fixed-point ((repeated average-damp times) fn)        
                 1.0)))




(display "16th root of 65536 ≈ ")
(nth-root 16 65536)

(display "11th root of 48828125 ≈ ")
(nth-root 11 48828125)


(newline)