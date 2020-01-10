#lang sicp

#|
problem asks specifically:
Write a procedure that examines a list and determines whether it contains a cycle, that is, whether a
program that tried to find the end of the list by taking successive cdrs would go into an infinite loop.

"by taking sucessive cdrs" means we don't need to worry about tree structures where the car contains
a cycle.

REVISIT -- make this work for trees?
|#

(define (contains-cycle? x)
  (define (terminus? x)
    (or (not (pair? x))
        (null? (cdr x))))
  (define (chase tortoise hare)
    (cond ((eq? tortoise hare) true)
          ((terminus? hare) false)
          (else
           (chase (cdr tortoise) (cddr hare)))))
  (if (terminus? x)
      false
      (chase x (cdr x))))




;; tests
(newline)
(define y0 '())
(display "(contains-cycle? ")
(display y0)
(display ")                              -> ")
(contains-cycle? y0)
(newline)

(define y1 '(1))
(display "(contains-cycle? ")
(display y1)
(display ")                             -> ")
(contains-cycle? y1)
(newline)

(define y2 '(1 2))
(display "(contains-cycle? ")
(display y2)
(display ")                           -> ")
(contains-cycle? y2)
(newline)

(define y3 '(1 2 3))
(display "(contains-cycle? ")
(display y3)
(display ")                         -> ")
(contains-cycle? y3)
(newline)


(set-cdr! (cdr y2) (cdr y2))
(display "(contains-cycle? ")
(display y2)
(display ")              -> ")
(contains-cycle? y2)
(newline)


(define x1 '(10))
(define x (append '(1 2 3 4 5 6 7 8 9) x1))
(display "(contains-cycle? ")
(display x)
(display ")          -> ")
(contains-cycle? x)
(newline)


(set-cdr! x1 x)
(display "(contains-cycle? ")
(display x)
(display ") -> ")
(contains-cycle? x)
(newline)


(define z1 '(6))
(define z (append '(1 2 3 4 5) z1))
(set-cdr! z1 (cddr z))
(display "(contains-cycle? ")
(display z)
(display ")      -> ")
(contains-cycle? z)
(newline)



