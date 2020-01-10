#lang sicp

(define (cube x) (* x x x))
(define (square x) (* x x))



(define (newtons-method g guess)
  (define (newton-transform g)
    (lambda (x)
      (- x (/ (g x) ((deriv g) x)))))
  (fixed-point (newton-transform g) guess))



(define (deriv f)
  (let ((dx 0.0000000001))
    (lambda (x)
      (/ (- (f (+ x dx)) (f x)) dx))))



(define (fixed-point f first-guess) 
  (define (close-enough? a b)
    (let ((tolerance 0.0000000001))
      (< (abs (- a b)) tolerance)))
  (define (try guess count)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next (+ count 1)))))
  (try first-guess 1))



(define (cubic a b c d)
  (display (pad-right col-one-width " " (string-append (polynomial->string a b c d) " ")))
  (lambda (x) (+ (* a (cube x))
                 (* b (square x))
                 (* c x)
                 d)))



(define (polynomial->string . coefficients)
  (define (term coeffs exp result)
    (if (< exp 0)
        result
        (let ((coeff (if (and (= 1 (abs (car coeffs)))
                              (not (null? (cdr coeffs))))
                         ""
                         (number->string (abs (car coeffs)))))
              (exponent (cond ((= exp 0) "")
                              ((= exp 1) "x")
                              (else (string-append "x^" (number->string exp)))))
              (negative? (< (car coeffs) 0)))
          
          (term (cdr coeffs)
                (- exp 1)
                (cond ((= (car coeffs) 0) result)
                      ((eq? result "")
                       (string-append (if negative? "-" "") coeff exponent))
                      (else (string-append result (if negative? " - " " + ") coeff exponent)))))))
  
  (term coefficients (- (length coefficients) 1) ""))



(define (pad-right n char str)
  (define (iter count result)
    (if (>= (string-length result) n)
        result
        (iter (+ count 1) (string-append result char))))
  (iter 0 str))



(define first-guess 1)
(define headings (cons "f(x)" (string-append "estimated root closest to " (number->string first-guess))))
(define col-one-width 28)

(newline)
(display (string-append (pad-right col-one-width " " (car headings)) (cdr headings)))
(newline)
(display (pad-right (+ col-one-width (string-length (cdr headings))) "-" ""))
(newline)

(newtons-method (cubic -1 0 0 0) first-guess)
(newtons-method (cubic 0 1 0 -1) first-guess)
(newtons-method (cubic 0 0 -45 0) first-guess)
(newtons-method (cubic 1 -2 2 5) first-guess)


(newline)