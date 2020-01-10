#lang sicp

; From it's definition (p. 38),    ϕ^2 = ϕ + 1
; Dividing both sides by ϕ,          ϕ = 1 + 1/ϕ,
; Therefore, ϕ is a fixed point of   x ↦ 1 + 1/x

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(display "\nϕ ≈ ") 
(fixed-point (lambda (x) (+ 1 (/ 1 x)))
             1.0)

(display "tolerance: ")
tolerance

(newline)