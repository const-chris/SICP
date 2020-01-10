#lang sicp
(newline)




(define (fringe items)
  (cond ((null? items) nil)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))
                       



(define x (list (list 1 2) (list 3 4)))


(display "x                   = ")
x

(display "(fringe x)          = ")
(fringe x)


(newline)


(display "(list x x)          = ")
(list x x)

(display "(fringe (list x x)  = ")
(fringe (list x x))




(newline)