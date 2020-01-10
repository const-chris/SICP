#lang sicp
(newline)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))




(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))




(define seq (list 1 3 0 5 0 1))
(display "seq                 = ")
seq

(display "(horner-eval 2 seq) = ")
(horner-eval 2 seq)




(newline)