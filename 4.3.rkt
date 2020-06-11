#lang sicp

(define (install-eval-procedures)
  (put 'eval 'quote  (lambda (exp env) (text-of-quotation exp)))
  (put 'eval 'set!   (lambda (exp env) (eval-assignment exp env)))
  (put 'eval 'define (lambda (exp env) (eval-definition exp env)))
  (put 'eval 'if     (lambda (exp env) (eval-if exp env)))
  (put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
  (put 'eval 'begin  (lambda (exp env) (eval-sequence (begin-actions exp) env)))
  (put 'eval 'cond   (lambda (exp env) (eval (cond->if exp) env))))


(install-eval-procedures)


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (exp-type exp)) => (lambda (f) (f exp env)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))


(define (exp-type exp) (car exp))
