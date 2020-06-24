#lang sicp
(#%require (file "./data-directed-table.rkt"))
(#%require (file "./evaluator.rkt"))
(#%provide (all-from (file "./data-directed-table.rkt"))
           (all-from (file "./evaluator.rkt"))
           (all-defined))
;; REVISIT -- trace through calls to for-each, p1, and p2 for the original lazy evaluator and Cy's

;; --------------- same as original ---------------
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((get 'eval (exp-type exp)) => (lambda (f) (f exp env)))
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
          (error "Unknown expression type: EVAL" exp))))

(define (exp-type exp)
  (if (variable? exp)
      'variable
      (car exp)))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)
             (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps)
                          env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps)
                      env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
            (actual-value
              input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (install-lazy-eval-procedures)
  (display "installing lazy 'eval procedures... ")
  (put 'eval 'variable (lambda (exp env) (lookup-variable-value exp env)))
  (put 'eval 'quote    (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set!     (lambda (exp env) (eval-assignment exp env)))
  (put 'eval 'define   (lambda (exp env) (eval-definition exp env)))
  (put 'eval 'if       (lambda (exp env) (eval-if exp env)))
  (put 'eval 'lambda   (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'eval 'begin    (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond     (lambda (exp env) (eval (cond->if exp) env))))

(install-lazy-eval-procedures)

;; ---------------- representing thunks ----------------
(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj)
                     result)             ; replace exp with its value
           (set-cdr! (cdr obj)
                     '())                ; forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))


;; Pick one:

;; ---------------- original lazy evaluator ----------------
(newline)
(display "using the original evaluator...")
(newline)
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

#| ;; -------------------- Cy's evaluator --------------------- |#
#| (newline) |#
#| (display "using Cy's evaluator...") |#
#| (newline) |#
#| (define (eval-sequence exps env) |#
#|   (cond ((last-exp? exps) (eval (first-exp exps) env)) |#
#|         (else |#
#|           (actual-value (first-exp exps) env)              ; changed |#
#|           (eval-sequence (rest-exps exps) env)))) |#


(define (run expr)
  (display expr)
  (display " -> ")
  (eval expr the-global-environment))

(newline)

;; a)
(run '(define (for-each proc items)
        (if (null? items)
            (begin
              (newline)
              'done)
            (begin (proc (car items))
                   (for-each proc (cdr items))))))

(run '(for-each (lambda (x) (newline) (display x))
                (list 57 321 88)))


(newline)
;; is it forced by the call to display in the lambda?
(run '(define y 0))

(run '(for-each (lambda (x) (set! y (+ y 1)))
                (list 57 321 88)))

(run 'y) ;; nope


(newline)
;; maybe inserting an internal definition containing a side-effect will screw it up
(run '(define z 0))

(run '(for-each (lambda (x)
                  (define (f)
                    (set! z (+ z 1))
                    0)
                  (f))
                (list 57 321 88)))

(run 'z) ;; nope


(newline)
;; what if the side-effect happens in the list?
(run '(define a 0))

(run '(for-each (lambda (x) (set! a (+ a 1)))
                (list 57 (set! a 42) 88)))

(run 'a) ;; nope, the list is forced by null?

#|
Because for-each is a higher-order procedure, its delayed procedure arguments are forced, as we saw in 4.28.
Moreover, it's second argument (the list) is forced by being passed to the primitive procedure null?.
Where the original lazy evaluator runs into trouble is a situation like p2 below, where a side-effect takes
place in the an argument that is not a procedure and isn't passed to a primitive procedure. In that case,
the side-effect is delayed by list-of-delayed-args, and it is never forced by being applied or by having a
primitive procedure applied to it.
|#


;; b)
(newline)
(run '(define (p1 x)
        (set! x (cons x '(2)))
        x))

(run '(define (p2 x)
        (define (p e)
          e
          x)
        (p (set! x (cons x '(2))))))

(run '(p1 1))

(run '(p2 1))

(run '(display (p2 1)))

#|
If we use the driver-loop, both evaluators behave the same here, maybe because of some racket-specific shenanigans??

However, running the evaluator directly, with the original lazy evaluator:
(p1 1) -> '(1 2)
(p2 1) -> <thunk> -> 1
(p2 1) returns a thunk, which we can view by passing it to user-print (or directly to display)

With Cy's evaluator:
(p1 1) -> '(1 2)
(p2 1) -> '(1 2)

In p2, with the original evaluator, when evaluating the inner call to p, a thunk representing the value of the expression
(set! x (cons x '(2))), created by list-of-delayed-args, is substituted for e. But e is never passed to any primitive
procedure, and so it is never forced. Since it is never forced, the assignment it represents doesn't ever happen, so x stays 1.
|#


#| c)
Cy's version behaves the same as the original with respect to for-each, because, as stated in a), a call to for-each
in the original evaluator is completely forced, because:
* its first argument is a procedure, which is forced when it is applied.
* its second argument is passed to the primitive procedure null?, which forces it.
|#


#| d) Laziness and assignment don't play well together. I prefer to keep my lazy language as lazy as
possible by default, and use monads for mutable state, etc. If I want assignment in my language, I
want it to be strict.

We don't need one language with all possible tools; that would be a mess. We need the right language
with the right abstractions for the task at hand.
|#
