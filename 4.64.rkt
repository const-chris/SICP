#lang sicp
(#%require (file "./microshaft-db.rkt"))

(run-query
  '(assert! (rule (outranked-by ?staff-person ?boss)
                (or (supervisor ?staff-person ?boss)
                    (and (outranked-by ?middle-manager ?boss)
                         (supervisor ?staff-person ?middle-manager))))))

(query-driver-loop)

#|
(outranked-by (Bitdiddle Ben) ?who)

drops into an infinite loop because when evaluating the conjunct compound query in the body of the rule

    (and (outranked-by ?middle-manager ?boss)
         (supervisor ?staff-person ?middle-manager))))))

the first simple query

    (outranked-by ?middle-manager ?boss)

needs to be evaluated in order for its stream of frames to be passed to the second part. ?middle-manager is a fresh (unbound) variable here, and to evaluate

    (outranked-by ?middle-manager ?boss)

another conjunct compound query of the form

    (and (outranked-by ?fresh-x ?boss)
         (supervisor ?middle-manager ?fresh-x))))))

would first need to be evaluated. Here all the variables are unbound. (?who is bound to ?boss, but ?who is itself a variable.) This recursion is obviously infinite.

Note: we can ignore the first branch of the disjunct compound query because branches of or queries are evaluated in parallel and merged after both branches are evaluated. The binding of ?boss to (Warbucks Oliver) in the first disjunct branch

    (supervisor ?staff-person ?boss)

does not affect the variable ?boss in the second branch.
|#
