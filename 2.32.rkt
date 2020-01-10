#lang sicp
(newline)




(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append (map (lambda (x) (cons (car s) x))
                     rest)
                rest))))


(define s (list 1 2 3))
(display "s           = ")
s


(display "(subsets s) = ")
(subsets s)




(display "

This procedure works on the same principle as the count-change procedure from chapter 1.
At each recursive step, we add (append) all the combinations that include the first element
to all the combinations that don't.

In the version given in the book, those two parts have their order reversed,
but the procedure also works (demonstrated) with the parts in the order described (the same order as
in the count-change procedure).

")