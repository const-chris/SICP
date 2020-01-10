#lang sicp
(newline)

(define (square x) (* x x))




(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))


(define (sq-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))




(define items (list 1 2 3 4))

(display "Louis's first implementation: (square-list items) = ")
(square-list items)
(display "
This produces a list in reverse order because as he cdrs down the list,
he conses each item to the front of his answer,
so the (square of the) last item seen is always first.




")


(display "Louis's second implementation: (square-list items) = ")
(sq-list items)
(display "
This doesn't work because the thing he is consing onto the front of
his answer is itself a list, so he gets a nested list of lists
instead of a list of numbers.

In other words, instead of


    (cons 1
          (cons 4
                (cons 9
                      (cons 16 nil))))


which is equivalent to (list 1 4 9 16), he gets


    (cons (cons (cons (cons nil 1)
                      4)
                9)
          16)
             
")




(newline)