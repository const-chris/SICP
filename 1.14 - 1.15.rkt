#lang scheme

; 1.14
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (display (string-append "cc " (number->string amount) " " (number->string kinds-of-coins)))
  (newline)
  (cond ((= amount 0) 1)
        ((or (< amount 0)
             (< kinds-of-coins 1))
         0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (denomination kinds-of-coins))
                     kinds-of-coins)))))

(define (denomination kinds)
  (cond ((= kinds 5) 50)
        ((= kinds 4) 25)
        ((= kinds 3) 10)
        ((= kinds 2) 5)
        ((= kinds 1) 1)))

(count-change 11)
(newline)
; For the recursive count-change procedure, space grows as O(amount), and number of steps grows as O(2^amount)



; 1.15
(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (cond ((<= (abs angle) 0.1) angle)
        (else
         (display "calling p")
         (newline)
         (p (sine (/ angle 3.0))))))

(sine 12.15)
; p is called five times, because in applicative-order evaluation the arguments are evaluated before they are passed to the function
; because we are reducing the magnitude of the angle by a factor of 3 before each call to p, and because each call to p increases the stack size by 1,
; both space and number of steps grow as O(log n)