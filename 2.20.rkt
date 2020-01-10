#lang sicp
(newline)




(define (same-parity x . ys)
  (let ((parity (remainder x 2)))
    (define (filter ys)
      (if (null? ys)
          nil          
          (if (= parity (remainder (car ys) 2))
              (cons (car ys) (filter (cdr ys)))
              (filter (cdr ys)))))
    (cons x (filter ys))))


            
(display "(same-parity 1 2 3 4 5 6 7) = ")
(same-parity 1 2 3 4 5 6 7)

(display "(same-parity 2 3 4 5 6 7) = ")
(same-parity 2 3 4 5 6 7)




(newline)

