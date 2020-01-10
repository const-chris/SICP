#lang racket
(require "square.rkt")
(require "table.rkt")
(provide get
         put
         print-table
         attach-tag
         type-tag
         contents
         apply-generic
         real-part
         imag-part
         magnitude
         angle
         make-complex-from-real-imag
         make-complex-from-mag-ang
         make-real
         make-integer
         numer
         denom
         make-rational
         add
         sub
         mul
         div
         equ?
         raise
         drop)

(define-values (get put print-table) (make-table))

(define (attach-tag type-tag contents) (cons type-tag contents))

(define (type-tag z)
  (if (pair? z)
      (car z)
      (error "Bad tagged datum -- TYPE-TAG" z)))

(define (contents z)
  (if (pair? z)
      (cdr z)
      (error "Bad tagged datum -- CONTENTS" z)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop (apply proc (map contents args)))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (let ((raise1 (raise a1))
                      (raise2 (raise a2)))
                  (cond (raise1
                         (drop (apply-generic op raise1 a2)))
                        (raise2
                         (drop (apply-generic op a1 raise2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))


(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang x y)
    (cons (* x (cos y))
          (* x (sin y))))
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag '(rectangular)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(rectangular)
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)


(define (install-polar-package)
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang x y) (cons x y))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag '(polar)
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang '(polar)
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (* x y))))
  ; (put 'div '(scheme-number scheme-number)
  ;  (lambda (x y) (tag (/ x y))))
  (put 'make 'integer (lambda (x) (tag (round x))))
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer '(rational) numer)
  (put 'denom '(rational) denom)
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


(define (numer x) (apply-generic 'numer x))
(define (denom x) (apply-generic 'denom x))

;(define (numer x) (car x))
;(define (denom x) (cdr x))

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (* x y))))
  (put 'div '(real real)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'real (lambda (x) (tag x)))
  'done)

(define (make-real n)
  ((get 'make 'real) n))


(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag '(rectangular)) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang '(polar)) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)


(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(install-integer-package)
(install-rational-package)
(install-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


(define (install-equ?-package)
  (define (eq-rat? x y)
    (let ((numer car)
          (denom cdr))
      (= (* (numer x) (denom y))
         (* (numer y) (denom x)))))
  (define (eq-complex? x y)
    (and (= (real-part x) (real-part y))
         (= (imag-part x) (imag-part y))))
  (put 'equ? '(integer integer) =)
  (put 'equ? '(rational rational) eq-rat?)
  (put 'equ? '(complex complex) eq-complex?)
  (put 'equ? '(real real) =)
  'done)

(install-equ?-package)
(define (equ? a b) (apply-generic 'equ? a b))


(define (install-raise-package)
  (define (raise-int x) (make-rational (contents x) 1))
  (define (raise-rat x) (make-real (/ (numer x) (denom x))))
  (define (raise-real x) (make-complex-from-real-imag (contents x) 0))
  (put 'raise 'integer raise-int)
  (put 'raise 'rational raise-rat)
  (put 'raise 'real raise-real)
  'done)

(define (raise x)
  (let ((r (get 'raise (type-tag x))))
    (if r
        (r x)
        false)))

(install-raise-package)


(define (install-project-package)
  (define (project-rat x) (make-integer (numer x)))
  (define (project-real x) (make-integer (round (contents x))))
  (define (project-complex x) (make-real (real-part x)))
  (put 'project 'rational project-rat)
  (put 'project 'real project-real)
  (put 'project 'complex project-complex)
  'done)

(install-project-package)

(define (project x)
  (let ((p (get 'project (type-tag x))))
    (if p
        (p x)
        false)))

(define (drop x)
  (if (not (pair? x))
      x
      (let ((projection (project x)))
        (if (and projection (equ? (raise projection) x))
            (drop projection)
            x))))
