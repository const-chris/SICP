#lang sicp

#|
a) The problem with Louis's plan is that we have defined our evaluator to work by process of elimination.
Therefore we define a procedure application to be any compound expression that is not one of the expression
types checked for in the cond clauses above the one that checks for procedure applications.

If we put the application clause first, all compound expressions will be evaluated as procedure applications.
|#

#|
b) We would need to make the following changes to make Louis's idea work:
|#

(define (application? exp) (tagged-list? exp 'call))

(define (operator exp) (cadr exp))

(define (operands exp) (cddr exp))
