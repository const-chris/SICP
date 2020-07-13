#lang sicp
(#%require (file "./microshaft-db.rkt"))

(run-query '(assert! (rule (same ?x ?x))))

(run-query '(assert! (rule (can-replace ?person1 ?person2)
                           (and (job ?person1 ?job1)
                                (or (job ?person2 ?job1)
                                    (and (job ?person2 ?job2)
                                         (can-do-job ?job1 ?job2)))
                                (not (same ?person1 ?person2))))))

(query-driver-loop)

#|
a. all people who can replace Cy D. Fect:
(can-replace ?x (Fect Cy D))

(can-replace (Bitdiddle Ben) (Fect Cy D))
(can-replace (Hacker Alyssa P) (Fect Cy D))


b. all people who can replace someone who is being paid more than they are:
(and (can-replace ?person1 ?person2)
     (salary ?person1 ?salary1)
     (salary ?person2 ?salary2)
     (lisp-value < ?salary1 ?salary2))

(and (can-replace (Aull DeWitt) (Warbucks Oliver))
     (salary (Aull DeWitt) 25000)
     (salary (Warbucks Oliver) 150000)
     (lisp-value < 25000 150000))
(and (can-replace (Fect Cy D) (Hacker Alyssa P))
     (salary (Fect Cy D) 35000)
     (salary (Hacker Alyssa P) 40000)
     (lisp-value < 35000 40000))
|#
