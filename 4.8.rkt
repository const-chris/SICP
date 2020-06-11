#lang sicp

;; from text
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

#| example transformation

(define (fib n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

(define (fib n)
  ((lambda ()
     (define fib-iter
       (lambda (a b count)
         (if (= count 0)
             b
             (fib-iter (+ a b) a (- count 1)))))
     (fib-iter 1 0 n))))

;|#

;#|
(define (let->combination exp)
  (if (named-let? exp)
      (list                                              ;; an immediate call to
       (make-lambda '()                                  ;; a lambda of no parameters
                    (list                                ;; whose body consists of
                     (named-let-definition exp)          ;; a procedure definition
                     (named-let-application exp))))      ;; and a call to said procedure
      (cons (make-lambda (let-vars exp) (let-body exp))
            (let-vals exp))))


(define (named-let? exp) (symbol? (cadr exp)))

(define (named-let-definition exp)
  (list 'define
        (let-name exp)
        (make-lambda (let-vars exp) (let-body exp))))

(define (named-let-application exp)
  (cons (let-name exp)
        (let-vals exp)))

(define (let-name exp)
  (if (named-let? exp)
      (cadr exp)
      (error "NOT A NAMED LET EXPRESSION -- let-name" exp)))

(define (let-vars exp) (map car (let-declarations exp)))
(define (let-vals exp) (map cadr (let-declarations exp)))

(define (let-declarations exp)
  (if (named-let? exp)
      (caddr exp)
      (cadr exp)))

(define (let-body exp)
  (if (named-let? exp)
      (cdddr exp)
      (cddr exp)))
;|#


#| test (using above example transformation)

(define exp '(let fib-iter ((a 1)
                            (b 0)
                            (count n))
               (if (= count 0)
                   b
                   (fib-iter (+ a b) a (- count 1)))))

(let->combination exp)

(equal? (let->combination exp)
        '((lambda ()
            (define fib-iter
              (lambda (a b count)
                (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))))
            (fib-iter 1 0 n))))

;|#
