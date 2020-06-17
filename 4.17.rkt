#lang sicp
(#%require (file "./utils/span.rkt"))
(#%require (file "./4.16.rkt"))
(#%provide (all-from (file "./4.16.rkt"))
 (all-defined))

#|
original
(lambda <vars>
  (define u <e1>)
  (define v <e2>)
  <e3>)

scanned-out
(lambda <vars>
  (let ((u '*unassigned*)
        (v '*unassigned*))
    (set! u <e1>)
    (set! v <e2>)
    <e3>))

Comparing execution of the scanned-out version with the original, scanning-out the internal definitions creates a let expression,
which is converted first to a lambda expression. Evaluating this lambda expression entails extending its environment with a new frame
in which its formal parameters are bound to its arguments.

For correct programs these two structures are equivalent, since the frame added in the scanned-out version encapsulates the entire
inner scope of the outer lambda. The only difference between the two is that in the scanned-out version, <vars> are bound in the enclosing
environment rather than the environment in which <e3> is executing.
If u or v is a member of <vars>, in the original its binding is overwritten by the internal definition, and in the scanned-out version its
value is shadowed by the binding in the frame created by the inner lambda (which was created by transforming the 'let expression).
In either case, while evaluating <e3> the same values for u and v are available.
If neither u nor v is a member of <vars>, any lookup of u or v in the original finds the value associated with u or v in the current
environment, or in the scanned-out version, find the same values by looking through the current environment and proceeding to the enclosing
environment.

To accomplish something similar to scanning-out without constructing any extra frames, a transformation could use individual define
expressions to initialize the internal variables to '*unassigned*, then use set! as in the scanned-out version to assign values to all the
variables:
;|#

(define (scan-out-defines procedure-body)
  (define (unassign definition)
    (list (car definition) (cadr definition) ''*unassigned*))
  (let* ((separated-body (span definition? procedure-body))
         (defines (car separated-body)))
    (if (null? defines)
        procedure-body
        (let* ((body (cdr separated-body))
               (scanned-defines (map unassign defines))
               (assignments (map (lambda (definition) (cons 'set! (cdr definition))) defines)))
          (append scanned-defines
                  (append assignments body))))))

;; reinstall
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(display "reinstalling 'lambda package with updated make-procedure... ")
(put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))

#| test
(newline)

(display "(define exp '((define u 1) (+ u x)))
         (define proc (make-procedure '(x) exp the-global-environment))")
(define exp '((define u 1) (+ u x)))
(define proc (make-procedure '(x) exp the-global-environment))
(newline)

(display "(apply proc '(2)) = ")
(apply proc '(2))
;|#

