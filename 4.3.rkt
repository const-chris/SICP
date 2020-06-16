#lang sicp
(#%require (file "./data-directed-table.rkt"))
(#%require (file "./interpreter.rkt"))
(#%provide (all-from (file "./data-directed-table.rkt"))
           (all-from (file "./interpreter.rkt"))
           (all-defined))

;; -------------------- new eval --------------------
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((get 'eval (exp-type exp)) => (lambda (f) (f exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))

(define (exp-type exp)
  (if (variable? exp)
      'variable
      (car exp)))

;; -------------------- unchanged --------------------
(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type: APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
         (eval (first-exp exps) env)
         (eval-sequence (rest-exps exps) env))))

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

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

;; ----------- eval procedures package -----------
(define (install-eval-procedures)
  (display "installing 'eval procedures... ")
  (put 'eval 'variable (lambda (exp env) (lookup-variable-value exp env)))
  (put 'eval 'quote    (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set!     (lambda (exp env) (eval-assignment exp env)))
  (put 'eval 'define   (lambda (exp env) (eval-definition exp env)))
  (put 'eval 'if       (lambda (exp env) (eval-if exp env)))
  (put 'eval 'lambda   (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'eval 'begin    (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond     (lambda (exp env) (eval (cond->if exp) env))))

(install-eval-procedures)

#| ------------------- tests -------------------
(eval '1 the-global-environment)
(eval 'car the-global-environment)
;|#

