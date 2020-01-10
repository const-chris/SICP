#lang racket
(require "utils/square.rkt")
(require "utils/all.rkt")
(require "utils/zipWith.rkt")
(require "table.rkt")
(provide get
         put
         print-table
         attach-tag
         type-tag
         contents
         apply-generic
         make-scheme-number
         make-sparse-poly-term
         make-sparse-poly-termlist
         make-dense-poly-termlist
         make-polynomial
         =zero?
         add
         sub
         mul
         div)

(define-values (get put print-table) (make-table))

(define (attach-tag type-tag contents) (cons type-tag contents))

(define (type-tag z)
  (cond ((number? z) 'scheme-number)
        ((pair? z) (car z))
        (else
         (error "Bad tagged datum -- TYPE-TAG" z))))

(define (contents z)
  (cond ((number? z) z)
        ((pair? z) (cdr z))
        (else
         (error "Bad tagged datum -- CONTENTS" z))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types -- APPLY GENERIC" (list op type-tags))))))


(define (install-scheme-number-package)
  (put '=zero? '(scheme-number)
       (λ (x) (= x 0)))
  (put 'negate '(scheme-number)
       (λ (x) (* x -1)))       
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'make 'scheme-number identity)
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


(define (install-sparse-termlist-package)
  ;; internal procedures  
  (define (adjoin-term term terms)
    (if (=zero? (coeff term))
        terms
        (cons term terms)))
  (define the-empty-termlist '())
  (define (first-term termlist) (car termlist))
  (define (rest-terms termlist) (cdr termlist))
  (define (empty-termlist? termlist) (null? termlist))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (=zero?-sparse terms)
    (all =zero? (map coeff terms)))
  (define (add-terms L1 L2)
     (cond ((empty-termlist? L1) L2)
           ((empty-termlist? L2) L1)
           (else
            (let ((t1 (first-term L1))
                  (t2 (first-term L2)))
              (cond ((> (order t1) (order t2))
                     (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                    ((< (order t1) (order t2))
                     (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                    (else
                     (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1)
                                 (rest-terms L2)))))))))

  (define (add-termlists L1 L2) (make-termlist (add-terms L1 L2)))

  (define (mul-terms L1 L2)
    (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
          the-empty-termlist
          (let ((t2 (first-term L)))
            (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms t1 (rest-terms L))))))
    (if (empty-termlist? L1)
        the-empty-termlist
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-termlists L1 L2) (make-termlist (mul-terms L1 L2)))
  
  (define (make-termlist terms) (tag terms))
  (define (tag p) (attach-tag 'sparse p))
  ;; interface to rest of system
  (put 'make-termlist '(sparse) make-termlist)
  (put 'termlist '(sparse)
       (lambda (p) (tag (cdr p))))
  (put 'empty-termlist? '(sparse) empty-termlist?)
  (put 'order '(sparse) order)
  (put 'coeff '(sparse) coeff)
  (put '=zero? '(sparse) =zero?-sparse)
  (put 'add-termlists '(sparse sparse) add-termlists)
  (put 'mul-termlists '(sparse sparse) mul-termlists)
  'done)


(define (install-dense-termlist-package)
  ;; internal procedures
  (define (termlist p) (tag (cdr p))) 
  (define (adjoin-term term termlist)
    (let ((order-term (order term))
          (order-list (- (length termlist) 1)))       
      (cond ((= order-term order-list)
             (cons (+ (coeff term) (car termlist)) ;; should be add instead of + for generic coefficients
                   (cdr termlist)))
            ((> order-term order-list)
             (adjoin-term term (cons 0 termlist)))
            (else
             (cons (car termlist)
                   (adjoin-term term (cdr termlist)))))))
  (define the-empty-termlist '())
  (define (first-term termlist)
    (make-term (- (length termlist) 1)
               (car termlist)))
  (define (rest-terms termlist) (cdr termlist))
  (define (empty-termlist? termlist) (null? termlist))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term)
    (if (pair? term)
        (cadr term)
        term))
  (define (=zero?-dense terms) (all =zero? terms))
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
                   (else (zipWith + L1 L2)))))))
  (define (add-termlists L1 L2) (make-termlist (add-terms L1 L2)))  
  (define (mul-terms L1 L2)
    (define (mul-term-by-all-terms t1 L)
      (if (empty-termlist? L)
          the-empty-termlist
          (let ((t2 (first-term L)))
            (adjoin-term
             (make-term (+ (order t1) (order t2))
                        (mul (coeff t1) (coeff t2)))
             (mul-term-by-all-terms t1 (rest-terms L))))))
    (if (empty-termlist? L1)
        the-empty-termlist
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-termlists L1 L2) (make-termlist (mul-terms L1 L2)))  
  (define (make-termlist terms) (tag terms))
  (define (tag p) (attach-tag 'dense p))
  ;; interface to rest of system
  (put 'make-termlist '(dense) make-termlist)
  (put 'termlist '(dense)
       (lambda (p) (tag (cdr p))))
  (put 'empty-termlist? '(dense) empty-termlist?)
  (put 'order '(dense) order)
  (put 'coeff '(dense) coeff)
  (put '=zero? '(dense) =zero?-dense)
  (put 'add-termlists '(dense dense) add-termlists)
  (put 'mul-termlists '(dense dense) mul-termlists)
  'done)

(define (install-polynomial-package)
  ;; imported
  (define (make-sparse-termlist terms)
    ((get 'make-termlist '(sparse)) terms))
  (define (make-dense-termlist terms)
    ((get 'make-termlist '(dense)) terms))
  (define (add-terms L1 L2) (apply-generic 'add-termlists L1 L2))
  (define (mul-terms L1 L2) (apply-generic 'mul-termlists L1 L2))
  ;; internal procedures
  (define (make-poly variable termlist) (cons variable termlist))
  (define (variable p) (car p))
  (define (termlist p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  (define (=zero-poly? p)
    (=zero? (termlist p)))
  (define (negate-poly p)
    (negate (termlist p)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (termlist p1) (termlist p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  
  (define (sub-poly x y)
    (add-poly x (negate-poly y))) 
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (termlist p1) (termlist p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  
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
  'done)


;(define (termlist p) (apply-generic 'termlist p))
;(define (empty-termlist? t) (apply-generic 'empty-termlist? t))
(define (first-term t) (apply-generic 'first-term t))
(define (rest-terms t) (apply-generic 'rest-terms t))
(define (coeff term) (apply-generic 'coeff term))
(define (order term) (car term))

(define (make-polynomial var termlist)
  ((get 'make 'polynomial) var termlist))

(define (make-sparse-poly-term order coeff) (list order coeff))
(define (make-sparse-poly-termlist terms)
  ((get 'make-termlist '(sparse)) terms))
(define (make-dense-poly-termlist terms)
  ((get 'make-termlist '(dense)) terms))


(define (=zero? x) (apply-generic '=zero? x))
(define (negate x) (apply-generic 'negate x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(install-scheme-number-package)
(install-sparse-termlist-package)
(install-dense-termlist-package)
(install-polynomial-package)




;#|
(newline)

(define t1 (make-sparse-poly-termlist '((12 1) (1 2) (0 1))))
;(display "t1 = ")
;t1

(define t2 (make-dense-poly-termlist '(2 3 4)))
;(display "t2 = ")
;t2

(define p1 (make-polynomial 'x t1))
(display "p1 = ")
p1

(define p2 (make-polynomial 'x t2))
(display "p2 = ")
p2

(newline)
(display "(add p1 p1) = ")
(add p1 p1)
(display "(add p2 p2) = ")
(add p2 p2)

(newline)
(display "(mul p1 p1) = ")
(mul p1 p1)
(display "(mul p2 p2) = ")
(mul p2 p2)


(newline)
;|#