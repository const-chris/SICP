#lang sicp
(#%require (file "./data-directed-table.rkt"))
(#%require (file "./evaluator.rkt"))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((get 'eval (exp-type exp)) => (lambda (f) (f exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)     ; changed actual-value to eval... this is a bad idea
                (operands exp)
                env))
        (else
          (error "Unknown expression type: EVAL" exp))))

;; everything else the same
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

(define (f op x y) (op x y))

(f + 1 1)

#|
In the above example (or any case in which procedure g is passed as an argument to procedure f, and then g is applied
in the body of f), the issue is that g will be an unevaluated thunk at call time, which matches neither primitive-procedure?
nor compound-procedure?, since it is tagged with 'thunk.

Example trace:

(apply (eval 'f env)
       '(+ 1 1)
       env)

(apply (list 'compound-procedure '(op x y) '((op x y)) env)
       '(+ 1 1)
       env)

(eval-sequence '((op x y))
               (extend-environment '(op x y)
                                   (list-of-delayed-args '(+ 1 1) env)
                                   env))

(eval-sequence '((op x y))
               (extend-environment '(op x y)
                                   (cons (delay-it '+ env)
                                         (list-of-delayed-args '(1 1) env))
                                   env))

(eval-sequence '((op x y))
               (extend-environment '(op x y)
                                   (cons (list 'thunk '+ env)
                                         (cons (list 'thunk 1 env)
                                               (list 'thunk 1 env)))
                                   env))

(eval '(op x y)
      (extend-environment '(op x y)
                          (list (list 'thunk '+ env)
                                (list 'thunk 1 env)
                                (list 'thunk 1 env))
                          env))

here we see the problem: '(op x y) is an application, but a list tagged with 'thunk doesn't match either of the
valid types in apply.

(apply (eval 'op extended-env)
       '(x y)
       extended-env)

(apply (list 'thunk '+ env)
       '(x y)
       extended-env)

<error!>

In the correct version of the lazy evaluator this is no problem, because we pass 'op to actual-value, which forces the
thunk before passing it on to apply.

(apply (actual-value 'op extended-env)
       '(x y)
       extended-env)

(apply (force-it (eval 'op extended-env))
       '(x y)
       extended-env)

(apply (force-it (list 'thunk '+ env))
       '(x y)
       extended-env)

(apply (actual-value (thunk-exp (list 'thunk '+ env))
                     (thunk-env (list 'thunk '+ env)))
       '(x y)
       extended-env)

(apply (actual-value '+ env)
       '(x y)
       extended-env)

(apply (force-it (eval '+ env))
       '(x y)
       extended-env)

(apply (force-it (list 'primitive +))
       '(x y)
       extended-env)

(apply (list 'primitive +)
       '(x y)
       extended-env)

|#
