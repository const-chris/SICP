#lang sicp
(#%require (file "./logic-evaluator.rkt"))

#|
disjoin and stream-flatmap interleave the streams rather than simply append them, because in the case of infinite streams interleaving ensures that, given enough time, all the results will be reached.

Take disjoin, for example. Without interleaving, if the stream of results returned by (qeval (first-disjunct disjuncts) frame-stream) is infinite, none of the other disjuncts will ever be evaluated.

Continuing our example from 4.71, we had:
|#

(run-query '(assert! (married Minnie Mickey)))

(run-query '(assert! (rule (married ?x ?y) (married ?y ?x))))

(run-query '(assert! (job Mickey (Disney character))))

(run-query '(assert! (job Minnie (Disney character))))

#|
If we ask:
|#

(run-query '(or (married Minnie ?who)
                (job ?who (Disney character))))

#|
we get:

(or (married Minnie Mickey) (job Mickey (Disney character)))
(or (married Minnie Minnie) (job Minnie (Disney character)))  ***
(or (married Minnie Mickey) (job Mickey (Disney character)))
(or (married Minnie Mickey) (job Mickey (Disney character)))
(or (married Minnie Mickey) (job Mickey (Disney character)))
...

Without interleaving, we'd never see that Minnie is a valid result for ?who.
The line marked by *** would never appear.
|#
