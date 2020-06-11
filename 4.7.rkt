#lang sicp

#| example transformation

(let* ((x 2)        (let ((x 2))
       (y 3)    =>    (let ((y 3))
       (z 4))           (let ((z 4))
  (+ x y z))              (+ x y z))))

|#


#| eval update and new predicate

(define (eval exp env)
  (cond
    ...
    ((let*? exp) (eval (let*->nested-lets exp) env))
    ...
    ))

(define (let*? exp) (tagged-list exp 'let*))

;|#


(define (let*->nested-lets exp)
  (make-nested-lets (let*-declarations exp) (let*-body exp)))

(define (let*-declarations exp) (cadr exp))
(define (let*-body exp) (cddr exp))


(define (make-nested-lets declarations body)
  (if (null? (cdr declarations))
      (make-let (car declarations) body)
      (make-let (car declarations)
                (make-nested-lets (cdr declarations) body))))


(define (make-let declaration body)
  (list 'let
        (list declaration)
        body))


;#| test

(define exp '(let* ((x 2) (y 3) (z 4)) (+ x y z)))

(let*->nested-lets exp)

;|#
