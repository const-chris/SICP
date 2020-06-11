#lang sicp

#|
If we search for the binding only in the first frame, there is no guarantee that the binding will be removed from the current scope.
Moreover, the only reason I can think of to want to remove a binding is to unshadow a binding in an enclosing frame.
With these two considerations in mind, it seems to make the most sense to search the environment frame by frame for the first occurrence
of the binding (as usual) and remove that.

This is a dangerous operation, and allowing it to unbind in outer scopes makes it unusably dangerous, but I can't see a purpose for it
if it can only unbind in the current scope.

I wouldn't implement this in a real language. It's either useless or unusable. An implementation for each version is shown below.
|#


(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan vars vals vars-tail vals-tail)
      (cond ((null? vars-tail)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars-tail))
             (set-cdr! vars (cdr vars-tail))
             (set-cdr! vals (cdr vals-tail)))
            (else (scan (cdr vars) (cdr vals) (cdr vars-tail) (cdr vals-tail)))))
    (if (not (eq? env the-empty-environment))
        (let* ((frame (first-frame env))
               (vars (frame-variables frame))
               (vals (frame-values frame)))
          (if (eq? var (car vars))
              (begin
                (set-car! frame (cdr vars))
                (set-cdr! frame (cdr vals)))
              (scan vars vals (cdr vars) (cdr vals))))))
  (env-loop env))


(define (make-unbound-in-current-frame! var env)
  (define (scan vars vals vars-tail vals-tail)
    (cond ((null? vars-tail) 'ok)
          ((eq? var (car vars-tail))
           (set-cdr! vars (cdr vars-tail))
           (set-cdr! vals (cdr vals-tail))
           'ok)
          (else (apply scan (map cdr (list vars vals vars-tail vals-tail))))))
  (if (not (eq? env the-empty-environment))
      (let* ((frame (first-frame env))
             (vars (frame-variables frame))
             (vals (frame-values frame)))
        (if (eq? var (car vars))
            (begin
              (set-car! frame (cdr vars))
              (set-cdr! frame (cdr vals))
              'ok)
            (scan vars vals (cdr vars) (cdr vals))))))

  


;; ----------- unchanged from text -----------
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))



;#| tests
(define env (extend-environment '(true false c) (list #t #f "world") the-empty-environment))
(set! env (extend-environment '(x y z) '(1 2 3) env))
(set! env (extend-environment '(a b c) '((1 2 3) "hello" "world") env))

#| whole-environment version
(make-unbound! 'x env)
(make-unbound! 'c env)
(make-unbound! 'missing env)
;|#

;#| current-frame-only version
(make-unbound-in-current-frame! 'a env)
(make-unbound-in-current-frame! 'x env)
(make-unbound-in-current-frame! 'c env)
(make-unbound-in-current-frame! 'missing env)
;|#

env
;|#