#lang sicp
(#%require (file "./microshaft-db.rkt"))

(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (let ((result-stream
              (qeval (unique-query operands)
                     (singleton-stream frame))))
        (if (singleton-stream? result-stream)
            result-stream
            the-empty-stream)))
    frame-stream))

(define (singleton-stream? stream)
  (and (pair? stream)
       (stream-null? (stream-cdr stream))))

(define (unique-query exps) (car exps))

(put 'unique 'qeval uniquely-asserted)

;#| tests
(run-query '(unique (job ?x (computer wizard))))

(run-query '(unique (job ?x (computer programmer))))

;; a query that lists all people who supervise precisely one person:
(run-query '(and (job ?x ?job)
                 (unique (supervisor ?underling ?x))))

#|
As shown above, we need a kind of "anchor" query to perform this kind of uniqueness check.
The anchor in this case, (job ?x ?job), serves to differentiate the various people who supervise only one person.
Perhaps a database schema containing assertions of the form (employee (Bitdiddle Ben)) would make this type of query a little cleaner.
We would then be able to construct queries like:

(and (employee ?x)
     (unique (supervisor ?underling ?x)))

whose results would contain a bit less redundant information.
In this case, they would look like:

(and (employee (Scrooge Eben)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
(and (employee (Hacker Alyssa P)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))

instead of:

(and (job (Scrooge Eben) (accounting chief accountant)) (unique (supervisor (Cratchet Robert) (Scrooge Eben))))
(and (job (Hacker Alyssa P) (computer programmer)) (unique (supervisor (Reasoner Louis) (Hacker Alyssa P))))
|#
;|#
