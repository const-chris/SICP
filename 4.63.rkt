#lang sicp
(#%require (file "./logic-evaluator.rkt"))
(#%provide (all-from (file "./logic-evaluator.rkt"))
           (all-defined))

(run-query '(assert! (son Adam Cain)))
(run-query '(assert! (son Cain Enoch)))
(run-query '(assert! (son Enoch Irad)))
(run-query '(assert! (son Irad Mehujael)))
(run-query '(assert! (son Mehujael Methushael)))
(run-query '(assert! (son Methushael Lamech)))
(run-query '(assert! (wife Lamech Ada)))
(run-query '(assert! (son Ada Jabal)))
(run-query '(assert! (son Ada Jubal)))

(run-query '(assert! (rule (grandson ?grandfather ?person)
                           (and (son ?grandfather ?father)
                                (son ?father ?person)))))

(run-query '(assert! (rule (son ?man ?person)
                           (and (wife ?man ?woman)
                                (son ?woman ?person)))))

#| tests
(newline)

(run-query '(grandson Cain ?x))

(run-query '(son Lamech ?x))

(run-query '(grandson Methushael ?x))
|#
