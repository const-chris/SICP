#lang scheme

; 1.16
(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (display (string-append (number->string a) " " (number->string b) " " (number->string n)))
    (newline)
    (if (<= n 0)
        a
        (if (odd? n)
            (fast-expt-iter (* a b) b (- n 1))
            (fast-expt-iter a (square b) (/ n 2)))))
  (fast-expt-iter 1 b n))

(define (square x) (* x x))

(fast-expt 2 2)
(newline)




; 1.17
(define (fast-multiply a b)
  (display (string-append (number->string a) " " (number->string b)))
  (newline)
  (if (= b 0)
      0
      (if (even? b)
          (fast-multiply (double a) (halve b))
          (+ a (fast-multiply a (- b 1))))))

(define (double x) (* x 2))

(define (halve x) (quotient x 2))

(fast-multiply 9 7)
(newline)




; 1.18
(define (faster-multiply a b)
  (define (fast-multiply-iter carry a b)
    (display (string-append (number->string carry) " " (number->string a) " " (number->string b)))
    (newline)
    (cond ((= b 0) 0)
          ((= b 1) (+ carry a))
          (else (if (even? b)
                    (fast-multiply-iter carry (double a) (halve b))
                    (fast-multiply-iter (+ carry a) a (- b 1))))))
  (fast-multiply-iter 0 a b))

(faster-multiply 9 7)
(newline)




; 1.19

; a' = (p + q)a + qb
; b' = qa + pb

; b'' = q((p + q)a + qb) + p(qa + pb)
; b'' = q(pa + qa + qb) + pqa + p^2b
; b'' = pqa + q^2a + q^2b + pqa + p^2b
; b'' = (pq + q^2 + pq)a + (q^2 + p^2)b
; b'' = (2pq + q^2)a + (q^2 + p^2)b
; b'' = q'a + p'b
; q' = 2pq + q^2
; p' = q^2 + p^2


(define (fib n)
  (define (fib-iter a b p q count)
    (display (string-append (number->string a) " " (number->string b) " " (number->string p) " " (number->string q) " " (number->string count)))
    (newline)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (+ (square p) (square q))
                     (+ (* 2 p q) (square q))
                     (/ count 2)))
          (else
           (fib-iter (+ (* p a) (* q a) (* q b))
                     (+ (* q a) (* p b))
                     p
                     q
                     (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 139)

