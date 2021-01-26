#lang racket

#|
The extra save and restore occur in afterfib-n-1:
    (restore continue)
    ...
    (save continue)
where nothing is done with the continue register or the stack in between these two instructions.

Also, in afterfib-n-2:
    (assign n (reg val))
    (restore val)
could be replaced by
    (restore n)
since we are simply adding these two values together (which is commutative, and therefore isn't concerned with which value is in which register), and since we immediately clobber val and either finish (if continue = fib-done) or clobber n (if continue = afterfib-n-1). (These are the only two possible values for continue here.)
|#
