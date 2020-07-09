#lang sicp

#|
Our parsing program relies on left-to-right evaluation of arguments, because parse-word scans
'*unparsed* left-to-right.

If our evaluator used some other evaluation order for aguments, we could encounter problems where,
for example, we are trying to parse a noun-phrase like "the cat," but (parse-word noun) is evaluated
before (parse-word article), and since the car of the unparsed phrase is not a noun, we fail.
|#
