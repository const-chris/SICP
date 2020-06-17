#lang sicp
(#%require (file "./4.16.rkt"))
(#%provide (all-from (file "./4.16.rkt"))
 (all-defined))

;; a)
(define (install-letrec-package)
  (define (letrec->let exp)
    (cons 'let
          (cons (unassigned-bindings exp)
                (append (assignments exp) (body exp)))))

  (define (unassigned-bindings exp)
    (map (lambda (var) (list var ''*unassigned*)) (letrec-names exp)))

  (define (letrec-names exp) (map car (letrec-bindings exp)))

  (define (assignments exp)
    (map (lambda (declaration) (cons 'set! declaration)) (letrec-bindings exp)))

  (define (letrec-bindings exp) (cadr exp))

  (define (body exp) (cddr exp))

  (display "installing letrec package... ")
  (put 'eval 'letrec (lambda (exp env) (eval (letrec->let exp) env))))

(install-letrec-package)

#| tests
(define exp '(letrec
               ((fact (lambda (n)
                        (if (= n 1) 1 (* n (fact (- n 1)))))))
               (fact 10)))

(eval exp the-global-environment)
;|#

#| b)
(define (f x)
  (letrec
    ((even? (lambda (n)
              (if (= n 0) true (odd? (- n 1)))))
     (odd? (lambda (n)
             (if (= n 0) false (even? (- n 1))))))
    ⟨rest of body of f⟩))

If we follow Louis's plan and use a plain let expression in place of the letrec expression, when that let
expression is converted to a lambda expression and applied to its arguments--the lambdas associated with even?
and odd?--these procedures are created in the enclosing environment (the one created by applying the lambda
constructed from the let expression to 5). So, when <rest of body f> calls either even? or odd? and its body is
evaluated, it is evaluated in the enclosing environment, which has no reference to the names even? and odd?.
|#
