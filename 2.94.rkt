#lang racket
(require "generic-arithmetic-polynomials.rkt")
(require "utils/all.rkt")
(provide greatest-common-divisor)


(define (install-polynomial-package-with-gcd)
  ;; internal procedures
  ;; new and updated
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))
  
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        a
        (gcd-terms b (remainder-terms a b))))

  (define (remainder-terms a b) (cadr (div-terms a b)))

  (define (sub-terms L1 L2)
    (if (empty-termlist? L2)
        L1  
        (add-terms L1 (map negate-term L2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-terms-result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car div-terms-result))
                (make-poly (variable p1) (cadr div-terms-result))))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((new-term (make-term new-o new-c)))
                  (let ((new-L1 (sub-terms L1 (mul-term-by-all-terms new-term L2))))
                    (let ((rest-of-result (div-terms new-L1 L2)))
                      (let ((quotient (car rest-of-result))
                            (remainder (cadr rest-of-result)))
                        (list (cons new-term quotient) remainder))))))))))
  
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero-poly? x) (all =zero? (map coeff (term-list x))))
  (define (negate-poly p)
    (make-poly (variable p)
               (map negate-term (term-list p))))
  (define (negate-term t)
    (make-term (order t) (negate (coeff t))))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (sub-poly x y)
    (add-poly x (negate-poly y))) 
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put '=zero? '(polynomial) =zero-poly?)
  (put 'negate '(polynomial)
       (lambda (p) (tag (negate-poly p)))) 
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))

  ;; new
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result)) (tag (cadr result)))))) 
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  'done)


;; inside scheme-number package
(put 'gcd '(scheme-number scheme-number) gcd)


(define (greatest-common-divisor a b) (apply-generic 'gcd a b))


;; tests
(newline)
(install-polynomial-package-with-gcd)
#|
(newline)


(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(display "p1 = ")
p1

(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(display "p2 = ")
p2

(newline)

(display "(greatest-common-divisor p1 p2) = ")
(greatest-common-divisor p1 p2)
;|#



(newline)
