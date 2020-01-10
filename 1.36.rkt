#lang sicp

(define tolerance 0.000000000001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess count)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (values next (string-append (number->string (+ count 1)) " steps"))
          (try next (+ count 1)))))
  (try first-guess 1))




(display "\nx^x = 1000\n")


(display "\n\nwithout average damping:\n\n")
(fixed-point (lambda (x) (/ (log 1000) (log x)))
             4.5)


(display "\n\nwith average damping:\n\n")
(fixed-point (lambda (x) (* 0.5(+ x (/ (log 1000) (log x)))))
             4.5)


(display "\n\ntolerance: ")
tolerance
(newline)