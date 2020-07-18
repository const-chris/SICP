#lang sicp
(#%require (file "./4.63.rkt"))
(#%require (file "./4.68.rkt"))

(run-query '(assert! (rule (ends-in-grandson ?list)
                           (append-to-form ?greats (grandson) ?list))))

(run-query '(assert! (rule ((grandson) ?x ?y)
                           (grandson ?x ?y))))

(run-query '(assert! (rule ((great . ?rel) ?x ?y)
                           (and (son ?x ?z)
                                (?rel ?z ?y)
                                (ends-in-grandson ?rel)))))

(run-query '(?rel Adam Jubal))

(run-query '(?rel Adam Irad))

(run-query '((great grandson) ?g ?ggs))

#|
This is a little ugly. Perhaps if we want to describe relationships between people using a list, we should be consistent:
((son) ?x ?y), ((grandson) ?x ?y), etc.
Not only would it remove duplicate results (see query below), but it would also obviate the need for the second rule above.
|#
(run-query '(?rel Adam Enoch))
;|#

