#lang sicp

(define (no-exps? seq) (null? seq))

(define (and? exp) (tagged-list exp 'and))
(define (or? exp) (tagged-list exp 'or))

(define (and-clauses exp) (cdr exp))
(define (or-clauses exp) (cdr exp))


;; special forms
(define (eval exp env)
  ((cond
     ...
     ((and? exp) (eval-and (and-clauses exp) env))
     ((or? exp) (eval-or (or-clauses exp) env))
     ...
     )))

(define (eval-and exps env)
  (cond ((no-exps? exps) 'true)
        ((last-exp? exps) (eval (first-exp exps) env))
        ((false? (eval (first-exp exps) env)) 'false)
        (else (eval-and (rest-exps exps) env))))

(define (eval-or exps env)
  (if (no-exps? exps)
      'false
      (let ((first-val (eval (first-exp exps) env)))
        (if first-val
            first-val
            (eval-or (rest-exps exps) env)))))


;; derived expressions
(define (eval exp env)
  ((cond
     ...
     ((and? exp) (eval (and->if exp) env))
     ((or? exp) (eval (or->if exp) env))
     ...
     )))

(define (and->if exp) (expand-and-clauses (and-clauses exp)))
  
(define (expand-and-clauses clauses)
  (cond ((null? clauses) 'true)
        ((null? (cdr clauses) (car clauses)))
        (else
         (make-if (car clauses)
                  (expand-and-clauses (cdr clauses))
                  'false))))

(define (or->if exp) (expand-or-clauses (or-clauses exp)))
  
(define (expand-or-clauses clauses)
  (cond ((null? clauses) 'false)
        (else
         (make-if (car clauses)
                  (car clauses)
                  (expand-and-clauses (cdr clauses))))))
