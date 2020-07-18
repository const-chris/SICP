#lang sicp
(#%require (file "./microshaft-db.rkt"))
(#%provide (all-from (file "./microshaft-db.rkt"))
           (all-defined))

(run-query
  '(assert! (rule (wheel ?person)
                  (and (supervisor ?middle-manager ?person)
                       (supervisor ?x ?middle-manager)))))

#| test
(newline)

(run-query '(wheel ?who))
;|#

#|
Oliver Warbucks appears four times here because there are four distinct frames for which (Warbucks Oliver) is the instantiation of the original query.
These frames are:

1. ?person: (Warbucks Oliver)
   ?middle-manager: (Scrooge Eben)
   ?x: (Cratchet Robert)

2. ?person: (Warbucks Oliver)
   ?middle-manager: (Bitdiddle Ben)
   ?x: (Hacker Alyssa P)

3. ?person: (Warbucks Oliver)
   ?middle-manager: (Bitdiddle Ben)
   ?x: (Fect Cy D)

4. ?person: (Warbucks Oliver)
   ?middle-manager: (Bitdiddle Ben)
   ?x: (Tweakit Lem E)
|#
