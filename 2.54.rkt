#lang sicp
(newline)




(define (equal? list1 list2)
  (define (sublist-equal? a b)
    (cond ((symbol? a) (and (symbol? b) (eq? a b)))
          ((number? a) (and (number? b) (= a b)))
          ((null? a) (null? b))
          (else (equal? a b))))

  (if (pair? list1)
      (and (pair? list2)
           (sublist-equal? (car list1) (car list2))
           (equal? (cdr list1) (cdr list2)))
      (sublist-equal? list1 list2)))
    
   


(display "(equal? '(this is a list) '(this is a list))   ")
(equal? '(this is a list) '(this is a list))
(display "(equal? 'apple 'apple)                         ")
(equal? 'apple 'apple)

(display "(equal? '(this is a list) '(this (is a) list)) ")
(equal? '(this is a list) '(this (is a) list))
(display "(equal? 'apple 'orange)                        ")
(equal? 'apple 'orange)

(display "(equal? '(this is 1 list) '(this is 1.0 list)) ")
(equal? '(this is 1 list) '(this is 1.0 list))
(display "(equal? '(this is 1 list) '(this is 3 list))   ")
(equal? '(this is 1 list) '(this is 3 list))

(display "(equal? '(1 2 3 4) '(1 2 3 4))                 ")
(equal? '(1 2 3 4) '(1 2 3 4))
(display "(equal? '(1 2 3 4) '(1 (2 3) 4))               ")
(equal? '(1 2 3 4) '(1 (2 3) 4))
(display "(equal? '(1 (2 3) 4) '(1 (2 3) 4))             ")
(equal? '(1 (2 3) 4) '(1 (2 3) 4))




(newline)
