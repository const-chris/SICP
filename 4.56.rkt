#lang sicp
(#%require (file "./microshaft-db.rkt"))

(query-driver-loop)

#|
the names of all people who are supervised by Ben Bitdiddle, together with their addresses:
  (and (supervisor ?x (Bitdiddle Ben))
       (address ?x ?y))

  (and (supervisor (Tweakit Lem E) (Bitdiddle Ben))
       (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
  (and (supervisor (Fect Cy D) (Bitdiddle Ben))
       (address (Fect Cy D) (Cambridge (Ames Street) 3)))
  (and (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
       (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))


all people whose salary is less than Ben Bitdiddle’s, together with their salary and Ben Bitdiddle’s salary:
  (and (salary (Bitdiddle Ben) ?z)
       (salary ?x ?y)
       (lisp-value < ?y ?z))

  (and (salary (Bitdiddle Ben) 60000)
       (salary (Aull DeWitt) 25000)
       (lisp-value < 25000 60000))
  (and (salary (Bitdiddle Ben) 60000)
       (salary (Cratchet Robert) 18000)
       (lisp-value < 18000 60000))
  (and (salary (Bitdiddle Ben) 60000)
       (salary (Reasoner Louis) 30000)
       (lisp-value < 30000 60000))
  (and (salary (Bitdiddle Ben) 60000)
       (salary (Tweakit Lem E) 25000)
       (lisp-value < 25000 60000))
  (and (salary (Bitdiddle Ben) 60000)
       (salary (Fect Cy D) 35000)
       (lisp-value < 35000 60000))
  (and (salary (Bitdiddle Ben) 60000)
       (salary (Hacker Alyssa P) 40000)
       (lisp-value < 40000 60000))


all people who are supervised by someone who is not in the computer division, together with the supervisor’s name and job:
  (and (supervisor ?x ?y)
       (not (job ?y (computer . ?type)))
       (job ?y ?z))

  (and (supervisor (Aull DeWitt) (Warbucks Oliver))
       (not (job (Warbucks Oliver) (computer . ?type)))
       (job (Warbucks Oliver) (administration big wheel)))
  (and (supervisor (Cratchet Robert) (Scrooge Eben))
       (not (job (Scrooge Eben) (computer . ?type)))
       (job (Scrooge Eben) (accounting chief accountant)))
  (and (supervisor (Scrooge Eben) (Warbucks Oliver))
       (not (job (Warbucks Oliver) (computer . ?type)))
       (job (Warbucks Oliver) (administration big wheel)))
  (and (supervisor (Bitdiddle Ben) (Warbucks Oliver))
       (not (job (Warbucks Oliver) (computer . ?type)))
       (job (Warbucks Oliver) (administration big wheel)))
|#
