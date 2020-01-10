#lang scheme

(define (square x)
  (* x x))

(define (pow base exponent)
  (define (pow-iter result count)
    (if (>= count exponent)
        result
        (pow-iter (* base result) (+ 1 count))))
  (pow-iter 1 0))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  ;  (< (abs(- (square guess) x)) 0.001))
  (< (abs(- (square guess) x)) (expt 10 -10)))
  
(define (improve guess x)
  (average guess (/ x guess)))

; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; (new-if (= 2 3) 0 5)
; (new-if (= 1 1) 0 5)

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                         x)))

(define (new-sqrt x)
  (new-sqrt-iter 1 x))

; (new-sqrt 2)
; Runs forever, because this interpreter uses applicative-order evaluation, so the arguments are evaluated before they are passed to the procedure.
; Since new-if is just a normal procedure, and since it is contained in the body of sqrt-iter, when the else-clause contains a recursive call to sqrt-iter,
; this sets off an infinite loop where the evaluator is continuously attempting to resolve identical recursive calls to sqrt-iter.



; 1.7
(define (sqrt-iter guess x)
  ;(display guess)
  ;(newline)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

;(define (sqrt x)
; (sqrt-iter 1 x))

; (sqrt 0.000001)
; Returns 0.0312... (correct answer is 0.001, so off by > 3000%)
; Fails for small numbers because with a fixed arithmetic interval for good-enough?, given a small enough value for x, the correct answer will be a very small fraction of the fixed interval

; (sqrt (pow 10.0 40))
; Runs forever, but very quickly converges to a good approximation (1.0000...e20)
; Because of the way numbers are represented in the computer, when the correct answer is larger than the largest number that can be stored,
; there is no guarantee that the answer will converge to the correct answer.
; It may be truncated and thus (suddenly) converge to a number that is not within the interval specified in good-enough?

(define (gooder-enough? guess previous-guess x)
  (let ((change (abs (- guess previous-guess))))
    (< (/ change guess) 0.0001)))

(define (gooder-sqrt-iter guess previous-guess x)
  (if (gooder-enough? guess previous-guess x)
      guess
      (gooder-sqrt-iter (improve guess x)
                        guess
                        x)))

(define (sqrt x)
  (if (= x 0)
      0
      (gooder-sqrt-iter (improve 1 x)
                        1
                        x)))

(sqrt 100.0)
(sqrt 0.0)
(sqrt 0.0000000001)
(sqrt (pow 10.0 40))
(newline)



; 1.8
(define (improve-cube-root guess x)
  (/
   (+
    (/ x (square guess))
    (* 2 guess))
   3))

(define (cube-root-iter guess previous-guess x)
  (if (gooder-enough? guess previous-guess x)
      guess
      (cube-root-iter (improve-cube-root guess x)
                      guess
                      x)))

(define (cube-root x)
  (if (= 0 x)
      0
      (cube-root-iter (improve-cube-root 1 x)
                      1
                      x)))

(cube-root 27.0)
(cube-root 8.0)
(cube-root (expt 10.0 9))
(cube-root 0.0)