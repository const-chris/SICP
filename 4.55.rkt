#lang sicp
(#%require (file "./microshaft-db.rkt"))

(query-driver-loop)

#|
all people supervised by Ben Bitdiddle:
  (supervisor ?x (Bitdiddle Ben))

  (supervisor (Tweakit Lem E) (Bitdiddle Ben))
  (supervisor (Fect Cy D) (Bitdiddle Ben))
  (supervisor (Hacker Alyssa P) (Bitdiddle Ben))


the names and jobs of all people in the accounting division:
  (job ?x (accounting . ?y))

  (job (Cratchet Robert) (accounting scrivener))
  (job (Scrooge Eben) (accounting chief accountant))


the names and addressed of all people who live in Slumerville:
  (address ?x (Slumerville . ?y))

  (address (Aull DeWitt) (Slumerville (Onion Square) 5))
  (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
  (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
|#
