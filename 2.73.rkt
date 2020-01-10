#lang racket
(require "symbolic-differentiation.rkt")
(require "table.rkt")
(require "2.56.rkt")

(define-values (get put print-table) (make-table))




(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                          var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

#|
a) In this version of the deriv procedure, we have outsourced the logic for dispatching on the operator
to the lookup procedure called by get. We can't extract the predicates number? and same-variable? into
the data-directed dispatch because get expects a pair of arguments, coordinates in the lookup table, and
for the cases where exp is a number or a variable, such coordinates don't exist--there are neither operator
nor operands in an expression like x or 3.


b)
;|#
(define (install-sum-package)
  (define (derivative operands var)
    (let ((addend (car operands))
          (augend (cadr operands)))
      (make-sum (deriv addend var)
                (deriv augend var))))

  (put 'deriv '+ derivative)
  'done)

(install-sum-package)


(define (install-product-package)
  (define (derivative operands var)
    (let ((multiplier (car operands))
          (multiplicand (cadr operands)))
      (make-sum (make-product multiplier
                              (deriv multiplicand var))
                (make-product (deriv multiplier var)
                              multiplicand))))

  (put 'deriv '* derivative)
  'done)

(install-product-package)


#|
c)
;|#
(define (install-exponential-package)
  (define (derivative operands var)
    (let ((base (car operands))
          (exponent (cadr operands)))
      (make-product (make-product exponent
                                  (make-exponentiation base (- exponent 1)))
                    (deriv base var))))

  (put 'deriv '** derivative)
  'done)

(install-exponential-package)
;|#


#|
d) If the indexing in the table were reversed, such that the dispatch line in deriv looked like

    ((get (operator exp) 'deriv) (operands exp) var)

the only changes that would need to be made would be to reverse the order of the first two arguments to
each of the put calls in the installation procedures.
;|#

(newline)




(display "(deriv '(** x 3) 'x) = ")
(deriv '(** x 3) 'x)

(display "(deriv '(** (* x (+ y 2)) 5) 'x) = ")
(deriv '(** (* x (+ y 2)) 5) 'x)




(newline)
