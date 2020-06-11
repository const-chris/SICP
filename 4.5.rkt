#lang sicp

;; We can no longer handle cond as a derived expression, so we need to change our dispatch in the evaluator:
(define (eval exp env)
  (cond
    ...
    ((cond? exp) (eval-cond exp env))
    ...
    ))


;; We also need a new predicate and a new selector for the arrow syntax:
(define (cond-arrow-clause? clause)
  (tagged-list (cond-actions clause) '=>))

(define (cond-arrow-recipient clause)
  (cdr (cond-actions clause)))


;; Then we can implement the cond evaluator:
(define (eval-cond exp env) (eval-cond-clauses (cond-clauses exp) env))

(define (eval-cond-clauses clauses env)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (eval-sequence (cond-actions first)) env)
                (error "ELSE clause isn't last: COND->IF" clauses))
            (let* ((predicate-value (eval (cond-predicate first) env)))
              (if (true? predicate-value)
                  (if (cond-arrow-clause? first)
                      (apply (eval (cond-arrow-recipient first) env)
                             ;; eww. tightly coupled to internal implementation of argument lists as scheme lists
                             ;; is something better possible?
                             (list predicate-value)) 
                      (eval-sequence (cond-actions first) env))
                  (eval-cond-clauses rest env))))))

 