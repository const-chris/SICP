#lang sicp
(newline)

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define numer car)
(define denom cdr)




(define (print-rat x)
  (newline)
  (cond ((= (numer x) 0) (display 0))
        ((= (denom x) 1) (display (numer x)))
        (else (display (numer x))
              (display "/")
              (display (denom x))))
  (newline))




(define (make-rat n d)
  (cond ((= d 0) (error "division by zero"))
        (else      
         (let ((g (gcd n d))
               (sign (if (> (* n d) 0)
                         1
                         -1)))           
           (cons (* sign (abs (/ n g)))
                 (abs (/ d g)))))))




(display "(make-rat 2 -10)")
(print-rat (make-rat 2 -10))

(display "(make-rat -2 -8)")
(print-rat (make-rat -2 -8))

(display "(make-rat -3 9)")
(print-rat (make-rat -3 9))

(display "(make-rat 27 3)")
(print-rat (make-rat 27 3))

(display "(make-rat -0 3)")
(print-rat (make-rat -0 3))

(display "(make-rat 1 -0)")
(print-rat (make-rat 1 -0))




(newline)