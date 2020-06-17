#lang sicp
(#%require (file "./utils/span.rkt"))
(#%require (file "./4.6.rkt"))
(#%provide (all-from (file "./4.6.rkt"))
           (all-defined))

;; a)
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (equal? (car vals) '*unassigned*)
                 (error "unsassigned variable -- LOOKUP-VARIABLE-VALUE" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; overwrite previous definition in table
(display "reinstalling 'variable package with updated lookup-variable-value... ")
(put 'eval 'variable (lambda (exp env) (lookup-variable-value exp env)))

#| tests
(newline)

(display "(eval '(car '(1 2 3)) the-global-environment) = ")
(eval '(car '(1 2 3)) the-global-environment)
(newline)

(display "(define-variable! 'y 42 the-global-environment)")
(define-variable! 'y 42 the-global-environment)
(newline)
(display "(eval 'y the-global-environment) = ")
(eval 'y the-global-environment)
(newline)

(display "(define-variable! 'x '*unassigned* the-global-environment)")
(define-variable! 'x '*unassigned* the-global-environment)
(newline)
(display "(eval 'x the-global-environment) = ")
(eval 'x the-global-environment)
(newline)
;|#


;; b)
(define (scan-out-defines procedure-body)
  (let* ((separated-body (span definition? procedure-body))
         (defines (car separated-body)))
    (if (null? defines)
        procedure-body
        (let* ((body (cdr separated-body))
               (let-vars (map cadr defines))
               (unassigned-declarations (map (lambda (var) (list var ''*unsassigned)) let-vars))
               (assignments (map (lambda (definition) (cons 'set! (cdr definition))) defines)))
          (list
           (cons 'let
                 (cons unassigned-declarations
                       (append assignments body))))))))

#| test
(newline)

(display "(define exp1 '((define u 1)
               (define v 2)
               (+ u v)))")

(define exp1 '((define u 1)
               (define v 2)
               (+ u v)))
(newline)

(display "(eval (car (scan-out-defines exp1)) the-global-environment) = ")
(eval (car (scan-out-defines exp1)) the-global-environment)
(newline)
;|#


;; c)
;; Scan out in make-procedure rather than procedure-body to avoid redundant computation
;; (procedure-body is called every time a procedure is applied).
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; overwrite previous definition in table
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
