#lang scheme

; 1.9
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

; (+ 5 2)
; (inc (+ 4 2))
; (inc (inc (+ 3 2)))
; (inc (inc (inc (+ 2 2))))
; (inc (inc (inc (inc (+ 1 2)))))
; (inc (inc (inc (inc (inc (+ 0 2))))))
; (inc (inc (inc (inc (inc 2)))))
; (inc (inc (inc (inc 3))))
; (inc (inc (inc 4)))
; (inc (inc 5))
; (inc 6)
; 7

; this is a recursive process


;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

; (+ 5 2)
; (+ (dec 5) (inc 2))
; (+ 4 3)
; (+ (dec 4) (inc 3))
; (+ 3 4)
; (+ (dec 3) (inc 4))
; (+ 2 5)
; (+ (dec 2) (inc 5))
; (+ 1 6)
; (+ (dec 1) (inc 6))
; (+ 0 7)
; 7

; this is an iterative process



; 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(display "(A 1 10): ")
(A 1 10)
(display "(A 2 4): ")
(A 2 4)
(display "(A 3 3): ")
(A 3 3)

(newline)

(define (f n) (A 0 n))
; f(n) = 2n
(display "(f 13): ")
(f 13)

(define (g n) (A 1 n))
; (A (- 1 1) (A 1 (- n 1)))
; (A 0 (A 1 (- n 1)))
; (* 2 (A 1 (- n 1)))
; (* 2 (* 2 (A 1 (- n 2))))
; (* (expt 2 (- n 1)) (A 1 (- n (- n 1))))   ; for positive integer n
; (* (expt 2 (- n 1)) (A 1 1))
; (* (expt 2 (- n 1)) 2)
; g(n) = 2^n
(display "(g 13): ")
(g 13)

(define (h n) (A 2 n))
; (A 1 (A 2 (- n 1)))
; (A 1 (A 1 (A 2 (- n 2))))
; (A 1 (A 1 (... {- n 1} ... (A 2 (- n (- n 1))) ... {- n 1} ... )))
; (A 1 (A 1 (... {- n 1} ... (A 2 1) ... {- n 1} ... )))
; (A 1 (A 1 (... {- n 1} ... 2 ... {- n 1} ... )))

; from g(n):
; (expt 2 (expt 2 (... {- n 1} ... 2 ... {- n 1} ... )))

; according to wikipedia, this is called tetration,
; so h(n) represents the nth tetration of 2

; h(n) = 2^(h(n - 1)), where h(0) = 1
(display "(h 4): ")
(h 4)

