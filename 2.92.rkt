#lang racket
(require "generic-arithmetic-polynomials.rkt")
(require "utils/flip.rkt")
(newline)


;; imported from install-polynomial-package
(define (make-poly variable term-list) (cons variable term-list))
(define (the-empty-termlist) '())
(define (empty-termlist? term-list) (null? term-list))
(define (variable p) (car p))
(define (term-list p) (cdr p))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (order term) (car term))
(define (coeff term) (cadr term))
(define (make-term order coeff) (list order coeff))
(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))
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
(define (tag p) (attach-tag 'polynomial p))


;; altered procedures
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1) (term-list p2)))
      (add-poly p1 (factor-as-polynomial-in (variable p1) p2))))


(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1) (term-list p2)))
      (mul-poly p1 (factor-as-polynomial-in (variable p1) p2))))


;; new procedures
(define lt? symbol<?)
(define (polynomial? x) (eq? (type-tag x) 'polynomial))


(define (distribute p terms) 
  (let ((var (variable p))
        (order-p (order (first-term (term-list p)))))
    (if (null? terms)
        '()
        (begin
          (let ((t1 (first-term terms)))
            (cons (make-term (order t1)
                             (make-polynomial var (list (list order-p (coeff t1)))))
                  (distribute p (rest-terms terms))))))))


(define (combine-terms terms)
  (let ((terms (sort-terms terms)))
    (define (iter terms prev-term result)
      (if (null? terms)
          (cons prev-term result)
          (let ((current-term (first-term terms)))
            (if (= (order prev-term) (order current-term))
                (let ((new-term (make-term (order prev-term)
                                           (add (coeff prev-term) (coeff current-term)))))
                  (iter (rest-terms terms) new-term result))
                (iter (rest-terms terms) current-term (adjoin-term prev-term result))))))
    (if (< (length terms) 2)
        terms
        (iter (rest-terms terms) (first-term terms) '()))))


(define (sort-terms terms)
  (define (insert-term term result)
    (cond ((null? result) (list term))
          ((>= (order term) (order (first-term result))) (adjoin-term term result))
          (else
           (adjoin-term (first-term result) (insert-term term (rest-terms result))))))
  (define (iter terms result)
    (if (null? terms)
        result
        (iter (rest-terms terms) (insert-term (first-term terms) result))))
  (iter terms '()))


(define (factor-as-polynomial-in var p)
  (let ((terms (term-list p)))
    (define (iter terms)
      (if (null? terms)
          '()
          (let ((t1 (first-term terms)))
            (let ((c1 (coeff t1))
                  (o1 (order t1)))             
              (if (and (polynomial? c1)
                       (same-variable? var (variable (contents c1))))
                  (begin      
                    (let ((c1-terms (term-list (contents c1))))
                      (let ((new-p (make-poly (variable p) (list (list o1 1)))))
                        (let ((expansion (distribute new-p c1-terms)))
                          (append expansion (iter (rest-terms terms)))
                          ))))
                  (cons t1 (rest-terms terms)))))))
    (make-poly var (sort-terms (combine-terms (iter terms))))))


(define (add-p-to-n p n)
  (tag (add-poly p
                 (make-poly (variable p)
                            (adjoin-term (make-term 0 n)
                                         (the-empty-termlist))))))

(define add-n-to-p (flip add-p-to-n))


(define (mul-p-by-n p n)
  (tag (mul-poly p
                 (make-poly (variable p)
                            (adjoin-term (make-term 0 n)
                                         (the-empty-termlist))))))

(define mul-n-by-p (flip mul-p-by-n))


(put 'add '(polynomial scheme-number) add-p-to-n)
(put 'add '(scheme-number polynomial) add-n-to-p)
(put 'add '(polynomial polynomial) (λ (p1 p2) (tag (add-poly p1 p2))))
(put 'mul '(polynomial scheme-number) mul-p-by-n)
(put 'mul '(scheme-number polynomial) mul-n-by-p)
(put 'mul '(polynomial polynomial) (λ (p1 p2) (tag (mul-poly p1 p2))))


;; tests
(define t1 '((1 1) (0 1)))
(define p01 (make-polynomial 'x t1))
(define p02 (make-polynomial 'y t1))

(define p1 (make-polynomial 'x (list (make-term 2 p02) (make-term 0 2))))
(define p2 (make-polynomial 'y (list (make-term 2 p01) (make-term 0 2))))
(display "p1          = ")
p1
(display "p2          = ")
p2
(newline)
      

(display "(add p1 p2) = ")
(add p1 p2)
(display "(mul p1 p2) = ")
(mul p1 p2)



(newline)

