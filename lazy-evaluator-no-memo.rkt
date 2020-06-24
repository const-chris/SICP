#lang sicp
(#%require (file "./data-directed-table.rkt"))
(#%require (file "./evaluator.rkt"))
(#%provide (all-from (file "./data-directed-table.rkt"))
           (all-from (file "./evaluator.rkt"))
           (all-defined))

;; ---------------- modifying the evaluator ----------------
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
           (list-of-arg-values arguments env))) ; changed
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)     ; changed
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
  (if (true? (actual-value (if-predicate exp) env))  ; changed
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

(define (force-it obj)
 (if (thunk? obj)
  (actual-value (thunk-exp obj) (thunk-env obj))
  obj))
