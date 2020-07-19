#lang sicp
(#%require (file "./stream-utils.rkt"))
#| (#%require (file "./logic-evaluator.rkt")) |#

#|
The purpose of the let statements in add-assertion! and add-rule! is to provide a new name by which to refer to a stream whose original name we want to set! to.
Since in the set! statement we want to stream-cons a new value onto the original stream, if we did not have a new name by which to refer to the stream, we would create an infinite stream of copies of the new value, as we did in defining the infinite stream of ones in chapter 3.

Since stream-cons delays its second argument, THE-ASSERTIONS is not evaluated at the time of the set!.
Rather, a promise to evaluate THE-ASSERTIONS is placed in the cdr of THE-ASSERTIONS.
When the stream-cdr is evaluated later, THE-ASSERTIONS will point to a stream whose stream-car is whatever the new value was, and whose stream-cdr is that same pointer.

In other words, because cons-stream delays its second argument, if we use the same name in the set! variable as in the second argument to cons-stream, we create a recursive definition for the variable, as demonstrated below:
|#

(define THE-ASSERTIONS (make-stream 'old-value))

(set! THE-ASSERTIONS (cons-stream 'new-value THE-ASSERTIONS))

(stream-take 10 THE-ASSERTIONS)

#| If we rename the variable first, however, our definition is not recursive and retains the pointer to the old value of the stream, like so: |#

(define THE-ASSERTIONS* (make-stream 'old-value))

(let ((old-assertions THE-ASSERTIONS*))
  (set! THE-ASSERTIONS* (cons-stream 'new-value old-assertions)))

(stream-take 2 THE-ASSERTIONS*)
