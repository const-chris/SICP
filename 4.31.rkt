#lang sicp
(#%require (file "./data-directed-table.rkt"))
(#%require (file "./evaluator.rkt"))
(#%provide (all-from (file "./data-directed-table.rkt"))
           (all-from (file "./evaluator.rkt"))
           (all-defined))


;; --------------------- updated apply ---------------------
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (let ((params (procedure-parameters procedure)))
           (eval-sequence
             (procedure-body procedure)
             (extend-environment
               params
               (list-of-compound-proc-args params arguments env)     ; changed
               (procedure-environment procedure)))))
        (else (error "Unknown procedure type: APPLY"
                     procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-compound-proc-args params args env)
  (if (no-operands? args)
      '()
      (let ((first-arg (first-operand args))
            (first-param (first-operand params))
            (rest  (list-of-compound-proc-args (rest-operands params) (rest-operands args) env)))
        (cond ((delay-arg? first-param)
               (cons (delay-it first-arg env) rest))
              ((delay-memo-arg? first-param)
               (cons (delay-it-memo first-arg env) rest))
              (else
                (cons (actual-value first-arg env) rest))))))

(define (delay-arg? param) (tagged-list? param 'lazy))
(define (delay-memo-arg? param) (tagged-list? param 'lazy-memo))


;; --------------- updated definition handling ---------------
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (map parse-param (cdadr exp))   ; formal parameters
                   (cddr exp))))                   ; body

(define (parse-param expr)
  (cond ((not (pair? expr)) expr)
        ((lazy-param? expr) (list 'lazy (parameter-body expr)))
        ((lazy-memo-param? expr) (list 'lazy-memo (parameter-body expr)))
        (else expr)))

(define (parameter-body expr) (car expr))
(define (lazy-param? arg) (eq? (cadr arg) 'lazy))
(define (lazy-memo-param? arg) (eq? (cadr arg) 'lazy-memo))


;; -------------- updated thunk representations --------------
(define (delay-it exp env)
  (list 'thunk exp env))

(define (delay-it-memo exp env)
  (list 'memo-thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (memo-thunk? obj)
  (tagged-list? obj 'memo-thunk))

(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) thunk-env obj))
        ((memo-thunk? obj)
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


;; ---------- unchanged from originial lazy evaluator ----------
;; --------------------- eval- procedures ----------------------
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

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
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


;; ------------------------ driver loop ------------------------
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


;; ----------------------- installation ------------------------
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

