#lang sicp
;; REVISIT -- test with complete evaluator?

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)


;; left-to-right evaluation
(define (ltr-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (cons first
              (ltr-list-of-values (rest-operands exps) env)))))


;; right-to-left evaluation
(define (rtl-list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (rtl-list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env)
              rest))))





;; Proof of concept ... for applications:
(define x 0)

(define (rtl exps)
  (if (no-operands? exps)
      '()
      (let ((rest (rtl (rest-operands exps))))
        (cons ((first-operand exps)) ;; apply the expr -- must be a procedure
              rest))))

(rtl (list
      (lambda ()
        (set! x (* x 2))
        1)
      (lambda ()
        (set! x (+ x 1))
        2)))

x

;; It's difficult to test for evaluation since scheme eagerly evaluates the list 'exps' when the list-of-values
;; procedure is called.

;; Proof that it doesn't work for evaluation:
(define y 0)

(define (rtl2 exps)
  (display y)
  (newline)
  (if (no-operands? exps)
      '()
      (let ((rest (rtl2 (rest-operands exps))))
        (cons (first-operand exps)
              rest))))

(rtl2 (list
       (begin
         (display "I happen first, no matter what")
         (newline)
         (set! y (* y 2))
         1)
       (begin
         (set! y (+ y 1))
         2)))

y

;; This clearly cannot work when the list-of-expressions procedure is applied in Scheme.
;; In order for the procedure body to affect the evaluation order, the implementation language must be lazy
;; (normal order evaluation)