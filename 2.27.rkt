#lang sicp
(newline)

(define (reverse x)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (cons (car rest)
                    result)
              (cdr rest))))
  (iter nil x))





(define (deep-reverse x)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (cons (reverse (car rest))
                    result)
              (cdr rest))))
  (iter nil x))


(define x (list (list 1 2) (list 3 4)))


(display "x                = ")
x

(display "(reverse x)      = ")
(reverse x)

(display "(deep-reverse x) = ")
(deep-reverse x)




(newline)