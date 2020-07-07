#lang sicp
(#%require (file "./4.35.rkt"))
(#%provide (all-from (file "./4.35.rkt"))
           (all-defined))

(input-definition '(define (distinct? items)
                       (cond ((null? items) true)
                             ((null? (cdr items)) true)
                             ((member (car items) (cdr items)) false)
                             (else (distinct? (cdr items))))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
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
          (list 'smith smith))))

(define (multiple-dwelling*)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

#|
It is obvious that the order of requirements does not affect the outcome.
It does, however, affect the running time.
To speed up the algorithm, we want to weed out failing cases as quickly as possible.

If we examine only the four requirements marked with a (*) above, it is clear that we prefer for
the two fletcher requirements to happen first, because after the first fletcher requirement runs,
we have eliminated 1/5 of all the possibilities, and running the second one eliminates 1/4 of the
remaining possibilities. Contrast that with, for example, running one fletcher requirement followed
by the baker requirement, which would eliminated 1/5 of all possibilities + 1/5 of the remaining
possibilities.

Similarly, the (> miller cooper) requirement gets priority because it eliminates 3/5 of all
possibilities.

The two non-adjacent floor requirements each eliminate ~8/25 possibilities, depending on where in
the order they are placed, so they go next.

The distinct? requirement is trickier: it eliminates (5^5 - 5!)/(5^5), or ~ 96% of the possibilities,
but it also runs in O(n!) time, as opposed to O(1) time like the rest of the requirements. Because
its time complexity is so much worse, it goes last.

It is worth noting that these changes make only a minor improvement to the running time. My testing
indicates it is ~20% faster with the changes to requirement order. See 4.40 for bigger improvements.
|#

(define (iterate n proc)
  (if (zero? n)
      'done
      (begin
        (proc)
        (iterate (- n 1) proc))))

(define (time algorithm)
  (let ((start-time (runtime)))
    (iterate 50 algorithm)
    (display (- (runtime) start-time))
    (newline)))

#| (time multiple-dwelling) |#
#| (time multiple-dwelling*) |#
