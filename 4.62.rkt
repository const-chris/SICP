#lang sicp
(#%require (file "./logic-evaluator.rkt"))
;; REVISIT -- explain why the order of the rules matters for the output of (last-pair ?x (3))

(run-query
  '(assert!
     (rule (last-pair (?head . ?tail) (?x))
           (last-pair ?tail (?x)))))

(run-query
  '(assert!
     (rule (last-pair (?x) (?x)))))

#|
(last-pair (3) ?x)

(last-pair (3) (3))
as expected


(last-pair (1 2 3) ?x)

(last-pair (1 2 3) (3))
as expected


(last-pair (2 ?x) (3))

(last-pair (2 3) (3))
as expected


(last-pair ?x (3))
runs forever, because there are infinitely many solutions
Interestingly, when the rules are input in the order above, the results are printed:

(last-pair (3) (3))
(last-pair (?head-2 3) (3))
(last-pair (?head-2 ?head-4 3) (3))
(last-pair (?head-2 ?head-4 ?head-6 3) (3))
(last-pair (?head-2 ?head-4 ?head-6 ?head-8 3) (3))
(last-pair (?head-2 ?head-4 ?head-6 ?head-8 ?head-10 3) (3))
etc.

But when the rules are input in the reverse order, no output is printed.
|#

(query-driver-loop)
