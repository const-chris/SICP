#lang racket
(newline)

;; representation of terms and term lists
  
(define (adjoin-term term term-list)
  (let ((order-term (order term))
        (order-list (- (length term-list) 1)))       
    (cond ((= order-term order-list)
           (cons (+ (coeff term) (car term-list)) ;; should be add instead of + for generic coefficients
                 (cdr term-list)))
          ((> order-term order-list)
           (adjoin-term term (cons 0 term-list)))
          (else
           (cons (car term-list)
                 (adjoin-term term (cdr term-list)))))))

(define (the-empty-termlist) '())

(define (first-term term-list)
  (make-term (- (length term-list) 1)
             (car term-list)))

(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))



(define ts '(3 2 0 0))                            ;; 3x^3 + 2x^2
(display "ts = ")
ts

(display
 "(adjoin-term (make-term (order (first-term ts))
                        (coeff (first-term ts)))
             (rest-terms ts)) = ")
(adjoin-term (make-term (order (first-term ts))
                        (coeff (first-term ts)))
             (rest-terms ts))                     ;; 3x^3 + 2x^2

(newline)

(define t1 (make-term 2 2))                       ;; 2x^2
(define t2 (make-term 5 1))                       ;; x^5
(define t3 (make-term 0 1))                       ;; 1

(display "t1 = ")
t1
(display "(adjoin-term t1 ts) = ")
(adjoin-term t1 ts)                               ;; 3x^3 + 4x^2
(newline)

(display "t2 = ")
t2
(display "(adjoin-term t2 ts) = ")
(adjoin-term t2 ts)                               ;; x^5 + 3x^3 + 2x^2
(newline)

(display "t3 = ")
t3
(display "(adjoin-term t3 ts) = ")
(adjoin-term t3 ts)                               ;; 3x^3 + 2x^2 + 1




(newline)
