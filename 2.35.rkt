#lang sicp
(newline)

(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))





(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x) (cond ((null? x) 0)
                                     ((pair? x) (count-leaves x))
                                     (else 1)))                
                   t)))




(define x (cons (list 1 2) (list 3 4)))
(define tree (list x x))

(display "tree = ")
tree

(display "(count-leaves tree) = ")
(count-leaves tree)




(newline)
       