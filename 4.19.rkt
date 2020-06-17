#lang sicp
;; REVISIT -- implement Eva's idea

;; expression:
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

#|
Ben: b = 11, a = 5, res: 16
Alyssa: b = (+ '*unassigned* 10) -> error, res: error
Eva: a = 5, b = 15, res: 20

In principle, I support Eva's position. As the footnote suggests, however, this is mechanistically challenging.
One possibility is to look for a definition in the current sequence when a variable is '*unassigned*. (Under our
transformation, this would mean searching the set! expressions in the body of the current let expression.) This would work
for this particular situation, but could lead to infinite recursions when two definitions mutually depend on each other.
If, for example, the above was:

...
(define b (+ a x))
(define a (+ b x))
...

we'd enter an infinite loop looking for a as a dependency of b and b as a dependency of a.

A possible workaround for this is to have two sentinel values, like '*unassigned*, and '*unassignable*. In the evaluation
phase in which we search the current sequence of expressions for definitions, once we have visited a variable, its value, if
'*unassigned*, is changed to '*unassignable* before we search for its dependencies, and if we ever encounter a variable whose
value is '*unassignable*, we know we have entered a loop, and terminate with an error.
It's certainly an imperfect, arguably inelegant solution.
|#

