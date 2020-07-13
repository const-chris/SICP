#lang sicp
(#%require (file "./microshaft-db.rkt"))

#|
The query

(lives-near ?person-1 ?person-2)

returns all possible ways of substituting for ?person-1 and ?person-2 such that the resulting
assertion is true. This means that, for example, both

(lives-near (Hacker Alyssa P) (Fect Cy D))
(lives-near (Fect Cy D) (Hacker Alyssa P))

will be in the result stream.

If we wanted to eliminate this kind of duplication, we'd need to create a query that differentiates ?person-1 from ?person-2 somehow.
One way to do this is using ordering. Since ?person-1 and ?person-2 are names, lexicographical ordering is a sensible choice. We could implement such a rule as follows:
|#


(run-query
  '(assert! (rule (same ?x ?x))))

(run-query
  '(assert!
     (rule (lives-near-ordered ?person-1 ?person-2)
           (and (address ?person-1 (?town . ?rest-1))
                (address ?person-2 (?town . ?rest-2))
                (not (same ?person-1 ?person-2))
                (lisp-value
                  (lambda (p1 p2)
                    (define (slist->string slst)
                      (cond ((null? slst) "")
                            ((null? (cdr slst)) (symbol->string (car slst)))
                            (else (string-append (symbol->string (car slst))
                                                 " "
                                                 (slist->string (cdr slst))))))
                    (string<? (slist->string p1) (slist->string p2)))
                  ?person-1 ?person-2)))))

#|
now running

(lives-near-ordered ?person-1 ?person-2)

yields

(lives-near-ordered (Aull DeWitt) (Reasoner Louis))
(lives-near-ordered (Aull DeWitt) (Bitdiddle Ben))
(lives-near-ordered (Fect Cy D) (Hacker Alyssa P))
(lives-near-ordered (Bitdiddle Ben) (Reasoner Louis))
|#

(query-driver-loop)
