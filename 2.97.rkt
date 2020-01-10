#lang racket
(require "generic-arithmetic-polynomials.rkt")
(require "utils/all.rkt")


(define (install-generic-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (=zero? x) (= (numer x) 0))
  (define (negate-rat x)
    (make-rat (* (numer x) -1) (denom x)))

  ;; updated
  (define (make-rat n d)
    (match-let ([(list reduced-n reduced-d) (reduce n d)])
      (cons reduced-n reduced-d)))
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put '=zero? '(rational) =zero?)
  (put 'negate '(rational) negate-rat)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)



(define (install-polynomial-package-with-gcd)
  ;; internal procedures
  ;; new and updated
  (define (n->terms n) (list (make-term 0 n)))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (match-let ([(list n d) (reduce-terms (term-list p1) (term-list p2))]
                    [var (variable p1)])
          (list (make-poly var n) (make-poly var d)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  
  (define (reduce-terms n d )
    (let* ((gcd-n-d (gcd-terms n d))
           (O1 (max (order (first-term n)) (order (first-term d))))
           (O2 (order (first-term gcd-n-d)))
           (integerizing-factor (expt (coeff (first-term gcd-n-d))
                                      (add 1 (sub O1 O2))))
           (n2 (quotient (div-terms (mul-terms n (n->terms integerizing-factor))
                                    gcd-n-d)))
           (d2 (quotient (div-terms (mul-terms d (n->terms integerizing-factor))
                                    gcd-n-d)))
           (coeffs (append (map coeff n2) (map coeff d2)))
           (gcd-coeffs (n->terms (foldl greatest-common-divisor (car coeffs) (cdr coeffs)))))       
      (list (quotient (div-terms n2 gcd-coeffs))
            (quotient (div-terms d2 gcd-coeffs)))))

   
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))
  
  (define (gcd-terms a b)
    (let* ((pseudo-gcd (if (empty-termlist? b)
                           a
                           (gcd-terms b (pseudoremainder-terms a b))))
           (coeffs-gcd (apply gcd (map coeff pseudo-gcd))))
      (quotient (div-terms pseudo-gcd (list (make-term 0 coeffs-gcd))))))

  (define (pseudoremainder-terms p q)
    (let* ((integerizing-factor
            (expt (coeff (first-term q))
                  (+ 1 (- (order (first-term p))
                          (order (first-term q))))))
           (int-fact-termlist (list (make-term 0 integerizing-factor))))
      (remainder (div-terms (mul-terms int-fact-termlist p) q))))

  (define (sub-terms L1 L2)
    (if (empty-termlist? L2)
        L1  
        (add-terms L1 (map negate-term L2))))

  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-terms-result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (quotient div-terms-result))
                (make-poly (variable p1) (remainder div-terms-result))))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))

  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let* ((new-c (div (coeff t1) (coeff t2)))
                     (new-o (- (order t1) (order t2)))
                     (new-term (make-term new-o new-c))
                     (new-L1 (sub-terms L1 (mul-term-by-all-terms new-term L2)))
                     (rest-of-result (div-terms new-L1 L2)))
                (list (cons new-term (quotient rest-of-result))
                      (remainder rest-of-result)))))))

  (define (quotient d) (car d))
  (define (remainder d) (cadr d))
  
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
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (list (tag (car result)) (tag (cadr result)))))) 
  (put 'gcd '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  ;; new
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((reduced (reduce-poly p1 p2)))
           (list (tag (car reduced)) (tag (cadr reduced))))))
  'done)


;; inside scheme-number package
(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'gcd '(scheme-number scheme-number) gcd)
(put 'reduce '(scheme-number scheme-number) reduce-integers)



(define (greatest-common-divisor a b) (apply-generic 'gcd a b))
(define (reduce a b) (apply-generic 'reduce a b))


;; tests
(newline)
(install-polynomial-package-with-gcd)
(install-generic-rational-package)
(newline)

(define p1 (make-polynomial 'x '((1 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1) (0 -1))))

(define rf1 (make-rational p1 p2))
(display "rf1           = ")
rf1

(define rf2 (make-rational p3 p4))
(display "rf2           = ")
rf2

(newline)

(display "(add rf1 rf2) = ")
(add rf1 rf2)


(newline)