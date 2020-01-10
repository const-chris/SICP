#lang sicp

(define (double f)
  (lambda (x) (f (f x))))

(display "
testing double: ((double inc) 0) = ")
((double inc) 0)

(display "


(((double (double double)) inc) 5)

expands to:

(((double (λ(x) (double (double x)))) inc) 5)
(((λ(x) (double (double (double (double x))))) inc) 5)

((double (double (double (double inc)))) 5)
((double (double (double add2))) 5)
((double (double add4)) 5)
((double add8) 5)
(add16 5) = ")
(((double (double double)) inc) 5)


(newline)