#lang sicp

;; from text
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))




#| additions to eval and new predicates

(define (eval exp env)
  (cond
    ...
    (while? (eval (while->lambda) env))
    (until? (eval (until->lambda) env))
    (do? (eval (do->lambda) env))
    (for? (eval (for->lambda) env)) 
    ...
    ))            

(define (while? exp) (tagged-list exp 'while))
(define (until? exp) (tagged-list exp 'until))
(define (do? exp) (tagged-list exp 'do))
(define (for? exp) (tagged-list exp 'for))

;|#




#| while

(define (range start end)
  ;; for simplicity of example, assume start < end
  (let ((current end)
        (res '()))
    (while (>= current start)
           (set! res (cons current res))
           (set! current (- current 1)))
    res))

(define (range start end)
  (let ((current end)
        (res '()))
    (((lambda (f)                                        ;; \
        ((lambda (procedure)                             ;; |
           (f (lambda () ((procedure procedure)))))      ;;  > applicative-order y-combinator 
         (lambda (procedure)                             ;; |
           (f (lambda () ((procedure procedure)))))))    ;; /
      (lambda (fn)
        (lambda ()
          (if (>= current start)
              (begin
                (set! res (cons current res))
                (set! current (- current 1))
                (fn))
              'false)))))
    res))
;|#


(define (while-predicate exp) (cadr exp))
(define (while-body exp) (cddr exp))

(define (while->lambda exp)
  (make-recursive
   (make-lambda '(fn)
                (list
                 (make-lambda '()
                              (list
                               (make-if (while-predicate exp)
                                        (append (sequence->exp (while-body exp))
                                                '((fn)))
                                        'false)))))))

(define (make-recursive proc)
  (list
   (list
    (make-lambda '(f)
                 (list
                  (list
                   (make-lambda '(procedure)
                                (list
                                 (list 'f
                                       (make-lambda '() '(((procedure procedure)))))))
                   (make-lambda '(procedure)
                                (list
                                 (list 'f
                                       (make-lambda '() '(((procedure procedure))))))))))
    proc)))
                
#| while test
(define exp '(while (>= current start)
                    (set! res (cons current res))
                    (set! current (- current 1))))

(while->lambda exp)
;|#




#| until

while with the predicate negated
|#

(define (until-predicate exp) (cadr exp))
(define (until-body exp) (cddr exp))

(define (until->lambda exp) (while->lambda (until->while exp)))

(define (until->while exp)
  (cons 'while
        (cons (negate (until-predicate exp))
              (until-body exp))))

(define (negate exp) (list 'not exp))

#| until test
(define exp '(until (< current start)
                    (set! res (cons current res))
                    (set! current (- current 1))))

(until->while exp)
(until->lambda exp)
;|#




#| do

similar to while, but testing the predicate happens after the body is executed, for example:

(define (range start end)
  ;; for simplicity of example, assume start < end
  (let ((current end)
        (res '()))
    (do ((set! res (cons current res))
         (set! current (- current 1)))
        (>= current start))
    res))

(define (range start end)
  (let ((current end)
        (res '()))
    (((lambda (f)
        ((lambda (procedure)
           (f (lambda () ((procedure procedure)))))
         (lambda (procedure)
           (f (lambda () ((procedure procedure)))))))
      (lambda (fn)
        (lambda ()
          (begin
            (set! res (cons current res))
            (set! current (- current 1)))
          (if (>= current start)
              (fn)
              'false)))))
    res))
;|#

(define (do-predicate exp) (caddr exp))
(define (do-body exp) (cadr exp))

(define (do->lambda exp)
  (make-recursive
   (make-lambda '(fn)
                (list
                 (make-lambda '()
                              (list
                               (sequence->exp (do-body exp))
                               (make-if (do-predicate exp) 
                                        '(fn)
                                        'false)))))))

#| do test
(define exp '(do ((set! res (cons current res))
                  (set! current (- current 1)))
               (>= current start)))

(do->lambda exp)
;|#




#| for

(define (range start end)
  ;; for simplicity of example, assume start < end
  (let ((res '()))
    (for ((current end)
          (>= current start)
          (set! current (- current 1)))
      (set! res (cons current res)))        
    res))

(define (range start end)
  (let ((res '()))
    (let ((current end))
      (((lambda (f)
          ((lambda (procedure)
             (f (lambda () ((procedure procedure)))))
           (lambda (procedure)
             (f (lambda () ((procedure procedure)))))))
        (lambda (fn)
          (lambda ()
            (if (>= current start)
                (begin
                  (set! res (cons current res))
                  (set! current (- current 1))
                  (fn))
                'false))))))
    res))
;|#

(define (for-init exp) (cadr exp))
(define (for-variable exp) (car (for-init exp)))
(define (for-predicate exp) (cadr (for-init exp)))
(define (for-increment exp) (caddr (for-init exp)))
(define (for-body exp) (cddr exp))

(define (for->lambda exp)
  (list
   'let
   (list (for-variable exp))
   (make-recursive
    (make-lambda '(fn)
                 (list
                  (make-lambda '()
                               (list
                                (make-if (for-predicate exp)
                                         (sequence->exp
                                          (append (for-body exp)
                                                  (list (for-increment exp)
                                                        '(fn))))
                                         'false))))))))

#| for test
(define exp '(for ((current end)
                   (>= current start)
                   (set! current (- current 1)))
               (set! res (cons current res))) )

(for->lambda exp)
;|#








;; more general Y-combinator-creator
;; suitable for functions of variable numbers of arguments
;; does not immediately call the resulting recursive procedure
(define (make-recursive-2 proc)
  (let ((args (get-args proc)))
    (list
     (make-lambda '(f)
                  (list
                   (list
                    (make-lambda '(procedure)
                                 (list
                                  (list 'f
                                        (make-lambda args (list (list 'apply '(procedure procedure) (cons 'list args)))))))
                    (make-lambda '(procedure)
                                 (list
                                  (list 'f
                                        (make-lambda args (list (list 'apply '(procedure procedure) (cons 'list args))))))))))
     proc)))

(define (get-args proc) (cadr (caddr proc)))


#| Y-combinator-creator test
(define F*
  '(lambda (func-arg)
     (lambda (n)
       (if (zero? n)
           1
           (* n (func-arg (- n 1)))))))

(make-recursive-2 F*)
(newline)


;; the result of (make-recursive-2 F*)

(define res ((lambda (f)
   ((lambda (procedure) (f (lambda (n) (apply (procedure procedure) (list n)))))
    (lambda (procedure) (f (lambda (n) (apply (procedure procedure) (list n)))))))
 (lambda (func-arg) (lambda (n) (if (zero? n) 1 (* n (func-arg (- n 1))))))))

(res 5)
;|#