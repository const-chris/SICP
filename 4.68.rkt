#lang sicp
(#%require (file "./logic-evaluator.rkt"))
(#%provide (all-from (file "./logic-evaluator.rkt"))
           (all-defined))

(run-query
  '(assert! (rule (append-to-form () ?y ?y))))

(run-query
  '(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
                  (append-to-form ?v ?y ?z))))

(run-query
  '(assert! (rule (reverse () ()))))

;#|
(run-query
  '(assert! (rule (reverse (?u . ?v) ?a)
                  (and (append-to-form ?b (?u) ?a)
                       (reverse ?v ?b)))))
;|#

#| Replacing the second reverse rule with the following inverts the issue described below (using both queries with the loop detector from 4.67 solves the issue, but returns duplicate results):
(run-query
  '(assert! (rule (reverse ?a (?u . ?v))
                  (and (append-to-form ?b (?u) ?a)
                       (reverse ?b ?v)))))
;|#

;#| This works for queries of the form:
(run-query '(reverse ?x (1 2 3)))
;|#

#| But it falls into an infinite loop for queries of the form:
(reverse (1 2 3) ?x)
;|#

