#lang sicp
(#%require (file "utils/apply-in-underlying-scheme.rkt"))
(#%require (file "interpreter.rkt"))

;; Installing map as a primitive fails because it is a higher-order function,
;; and apply-primitive-procedure is not designed to handle procedures as arguments.

;; In the world where Louis has installed map as a primitive, let's trace the execution of:
;; (1)
(eval (map inc '(1 2)) the-global-environment)
;; Assume inc is a primitive
;; Our expression is an application, so:
;; (2)
(apply (eval (operator '(map inc '(1 2))) the-global-environment)
       (list-of-values (operands '(map inc '(1 2))) the-global-environment))
;; (3)
(apply (eval 'map the-global-environment)
       (list-of-values '(inc '(1 2)) the-global-environment))
;; 'map is a symbol, so we find it with (lookup-variable-value 'map the-global-environment)
;; (4)
(apply (list 'primitive <map>)
       (list (list 'primitive <inc>) '(1 2)))
;; (5)
(apply-primitive-procedure (list 'primitive <map>)
                           (list (list 'primitive <inc>) '(1 2)))
;; (6)
(apply-in-underlying-scheme (primitive-implementation (list 'primitive <map>))
                            (list (list 'primitive <inc>) '(1 2)))
;; (7)
(apply-in-underlying-scheme <map> (list (list 'primitive <inc>) '(1 2)))
;; here we see a problem: the first argument is (list 'primitive ...)
;; map expects it's first argument to be a procedure, not a list

;; if inc is not a primitive, the trace is essentially the same until (4), which becomes:
(apply (list 'primitive <map>)
       (list (list 'procedure <inc>) '(1 2)))
;; and we can see that we will run into the same problem: the first argument to map will be
;; a list rather than a procedure




;; Now let's trace the same call in the world where map has been defined as a non-primitive
;; procedure:
(define (map op xs)
  (if (null? xs)
      '()
      (cons (op (car xs))
            (map op (cdr xs)))))

(eval (map inc '(1 2)) the-global-environment)
;; (2)
(apply (eval (operator '(map inc '(1 2))) the-global-environment)
       (list-of-values (operands '(map inc '(1 2))) the-global-environment))
;; (3)
(apply (eval 'map the-global-environment)
       (list-of-values '(inc '(1 2)) the-global-environment))
;; The traces start to diverge here:
;; (4)
(apply (list 'procedure <map>)
       (list (list 'primitive <inc>) (1 2)))
;; (5)
(eval-sequence (procedure-body (list 'procedure <map>))
               (extend-environment (procedure-parameters procedure)
                                   (list (list 'primitive <inc>) '(1 2))
                                   (procedure-environment (list 'procedure <map>))))
;; (6)
(eval-sequence '(if (null? xs)
                    '()
                    (cons (op (car xs))
                          (map op (cdr xs))))
               (extend-environment '(op xs)
                                   (list (list 'primitive <inc>) '(1 2))
                                   the-global-environment))
;; now eval-sequence will evaluate the body of map in an environment where it's parameters
;; are bound to the argument values, and, crucially, when it needs to evaluate op, it will
;; recursively call the evaluator, which will strip off the 'primitive (or 'procedure, if we
;; had defined inc as a non-primitive procedure) tag, and return a procedure that can be
;; applied to arguments, rather than a list.

;; For example, let's look at the first call to (op (car xs))
;; Denote the extended environment by <env>
(eval '(op 1) <env>)
;; Again our expression is an application:
(apply (eval 'op <env>)
       (list-of-values '(1) <env>))
;; We look up op in <env>:
(apply (list 'primitive <inc>) '(1))
;; And finally when we call apply-in-underlying-scheme,
(apply-in-underlying-scheme <inc> '(1))
;; everything works





















