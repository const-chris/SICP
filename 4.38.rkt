#lang sicp
(#%require (file "./4.35.rkt"))

#|
If we omit the restriction that Smith and Fletcher do not live on adjacent floors (as we've done in
multiple-dwelling* below), the number of solutions increases from 1 to 5.
|#

(input-definition '(define (distinct? items)
                     (cond ((null? items) true)
                           ((null? (cdr items)) true)
                           ((member (car items) (cdr items)) false)
                           (else (distinct? (cdr items))))))

(input-definition '(define (multiple-dwelling)
                     (let ((baker (amb 1 2 3 4 5))
                           (cooper (amb 1 2 3 4 5))
                           (fletcher (amb 1 2 3 4 5))
                           (miller (amb 1 2 3 4 5))
                           (smith (amb 1 2 3 4 5)))
                       (require
                         (distinct? (list baker cooper fletcher miller smith)))
                       (require (not (= baker 5)))
                       (require (not (= cooper 1)))
                       (require (not (= fletcher 5)))
                       (require (not (= fletcher 1)))
                       (require (> miller cooper))
                       (require (not (= (abs (- smith fletcher)) 1)))
                       (require (not (= (abs (- fletcher cooper)) 1)))
                       (list (list 'baker baker)
                             (list 'cooper cooper)
                             (list 'fletcher fletcher)
                             (list 'miller miller)
                             (list 'smith smith)))))

(input-definition '(define (multiple-dwelling*)
                     (let ((baker (amb 1 2 3 4 5))
                           (cooper (amb 1 2 3 4 5))
                           (fletcher (amb 1 2 3 4 5))
                           (miller (amb 1 2 3 4 5))
                           (smith (amb 1 2 3 4 5)))
                       (require
                         (distinct? (list baker cooper fletcher miller smith)))
                       (require (not (= baker 5)))
                       (require (not (= cooper 1)))
                       (require (not (= fletcher 5)))
                       (require (not (= fletcher 1)))
                       (require (> miller cooper))
                       (require (not (= (abs (- fletcher cooper)) 1)))
                       (list (list 'baker baker)
                             (list 'cooper cooper)
                             (list 'fletcher fletcher)
                             (list 'miller miller)
                             (list 'smith smith)))))
(driver-loop)
