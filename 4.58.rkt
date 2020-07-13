#lang sicp
(#%require (file "./microshaft-db.rkt"))

(run-query
 '(assert! (rule (outranked-by ?staff-person ?boss)
                 (or (supervisor ?staff-person ?boss)
                     (and (supervisor ?staff-person ?middle-manager)
                          (outranked-by ?middle-manager ?boss))))))

(run-query
 '(assert! (rule (big-shot-in ?x ?division)
                 (and (job ?x (?division . ?type))
                      (not (and (outranked-by ?x ?boss)
                                (job ?boss (?division . ?type2))))))))

(query-driver-loop)

#|
(big-shot-in ?person ?division)

(big-shot-in (Scrooge Eben) accounting)
(big-shot-in (Warbucks Oliver) administration)
(big-shot-in (Bitdiddle Ben) computer)
|#
