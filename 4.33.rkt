#lang sicp
(#%require (file "./lazy-evaluator-with-streams.rkt"))
(#%provide (all-from (file "./lazy-evaluator-with-streams.rkt"))
           (all-defined))

(define (eval-quote expr env)
 (if (pair? (text-of-quotation expr))
     (eval (make-lazy-list (text-of-quotation expr)) env)
     (text-of-quotation expr)))

(define (make-lazy-list expr)
  (if (null? expr)
      ''()
      (list 'cons
            (car expr)
            (make-lazy-list (cdr expr)))))

(display "installing lazy-list literals package... ")
(put 'eval 'quote (lambda (expr env) (eval-quote expr env)))
