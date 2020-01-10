#lang racket

(display "
Equivalent algebraic expressions may lead to different answers because when a variable representing an interval occurs more than once in an expression,
then the errors associated with each occurence are taken independently, and the resulting interval grows.

For expressions that can be rewritten such that each variable representing an interval occurs only once,
we can get an interval result that represents the exact error bounds of the expression.

However, not all expressions can be rewritten this way.

It may be possible to construct an interval-arithmetic package such that expressions are normalized to minimize repetition of variables in a deterministic way
before arithmetic operations are conducted. If so, then such a package would not have the problem that different algebraically equivalent expressions
produce different results. Devising such a package, if possible, is certainly beyond the scope of the SICP course.

; REVISIT -- (low priority) PACKAGE FOR REDUCING INTERVAL-ARITHMETIC EXPRESSIONS?

")
