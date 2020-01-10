#lang sicp
(newline)




(define (reverse x)
  (define (reverse-iter result rest)
    (if (null? rest)
        result
        (reverse-iter (cons (car rest)
                            result)
                      (cdr rest))))
  (reverse-iter nil x))


(display "(reverse (list 1 4 9 16 25)) = ")
(reverse (list 1 4 9 16 25))




(newline)