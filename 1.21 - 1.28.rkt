#lang sicp

1.21
(define (square x) (* x x))

(define (divides? x y)
  (= (remainder y x) 0))

(define (smallest-divisor n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (find-divisor 2))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)




(newline)
1.22
(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      0))

(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  1)

(define (search-for-primes start number)
  (cond ((< number 1) (newline))
        ((even? start) (search-for-primes (+ start 1) number))
        (else (search-for-primes (+ start 2) (- number (timed-prime-test start))))))

(search-for-primes 1009 3)
(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)
; (search-for-primes 10000000 3)
; There's considerable variance, but the data seem to bear out the idea that prime? grows as O(n^(1/2)).
; Therefore the result is compatible with the notions that programs on this machine run in time proportional to the number of steps required for the computation.
; IDEA: WRITE A STREAM PROCESSING PROCEDURE THAT COMPUTES THE AVERAGE TIME TO FIND EACH PRIME IN THE GIVEN RANGE OVER A GIVEN NUMBER OF TRIALS




(newline)
1.23
(define (smallest-divisor-2 n)
  (define (find-divisor test-divisor)
    (let ((next
            (if (= test-divisor 2)
                3
                (+ test-divisor 2))))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor next)))))
  (find-divisor 2))

(define (prime?-2 n)
  (= (smallest-divisor-2 n) n))

(define (timed-prime-test-2 n)
  (start-prime-test-2 n (runtime)))

(define (start-prime-test-2 n start-time)
  (if (prime?-2 n)
      (report-prime n (- (runtime) start-time))
      0))

(define (search-for-primes-2 start number)
  (cond ((< number 1) (newline))
        ((even? start) (search-for-primes-2 (+ start 1) number))
        (else (search-for-primes-2 (+ start 2) (- number (timed-prime-test-2 start))))))

(search-for-primes-2 1000 3)
(search-for-primes-2 10000 3)
(search-for-primes-2 100000 3)
(search-for-primes-2 1000000 3)
; (search-for-primes-2 10000000 3)
; The ratio appears to be 3:2, rather than 2:1
; Presumably, this is because of the added procedure call to next and the added if statement contained in that procedure;
; Inlining the logic for next gets us closer to 2:1
; Similarly, using a let statement to define next gets us closer to 2:1
; Clearly the step of calling a procedure is heavy




(newline)
1.24
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
          (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test-3 n)
  (start-prime-test-3 n (runtime)))

(define (start-prime-test-3 n start-time)
  (if (fast-prime? n 10000)
      (report-prime n (- (runtime) start-time))
      0))

(define (search-for-primes-3 start number)
  (cond ((< number 1) (newline))
        ((even? start) (search-for-primes-3 (+ start 1) number))
        (else (search-for-primes-3 (+ start 2) (- number (timed-prime-test-3 start))))))

; (search-for-primes-3 2 3)
(search-for-primes-3 1000 3)
(search-for-primes-3 10000 3)
(search-for-primes-3 100000 3)
(search-for-primes-3 1000000 3)
; (search-for-primes-3 10000000 3)
; (search-for-primes-3 100000000 3)
; (search-for-primes-3 1000000000 3)

; We expect, based on O(log n) growth, that finding primes around 1e6 should take twice as long as finding primes around 1e3.
; The results show a ratio a bit lower than that.
; Again this is most likely because calling procedures takes a significant amount of time,
; and so calling fermat-test a fixed number of times increases the time to find any prime, no matter the size, by a constant amount.
; We can see this by searching for the first three primes greater than 2, which are consecutive odd integers and so the search happens in O(1).
; If we subtract the time to find these first primes from all the other times, we find that the ratio is quite close to 2:1.
; If we look at larger numbers, we see that the ratios jump between 1e6 and 1e7
; This must be the range in which the internal representations of numbers change from, say, num to big-num, and calculations on big-nums must be less efficient
; THE DATA IS EXTREMELY NOISY, REVISIT AND TRY WITH STREAM PROCESSING TO BRING VARIANCE DOWN




; 1.25
; (define (fast-exp base exp)
;  (cond ((= exp 0) 1)
;        ((even? exp) (square (fast-exp base (/ exp 2))))
;        (else (* base (fast-exp base (- exp 1))))))

; (define (expmod base exp m)
;   (remainder (fast-exp base exp) m))

; This is very slow.
; Because we are computing fast-exp, the numbers get very, very large.
; When computing (fermat-test n), we must deal with numbers on the order of n^n.



(newline)
1.26
(define (louis-expmod base exp m)
  (display "called")
  (newline)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (louis-expmod base (/ exp 2) m)
                       (louis-expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base
                        (louis-expmod base (- exp 1) m))
                     m))))

(louis-expmod 4 4 5)
; This defeats the purpose of reducing the exponent by half when it is even, since every time we do that, we make two recursive calls to expmod.
; Thus it takes an O(log n) algorithm and returns it back to its unoptimized O(n) beginnings.




(newline)
1.27
(define (p n)
  (define (inner-p a)
    (cond ((= 0 a) true)
          ((= (expmod a n n) a)
           (inner-p (- a 1)))
          (else false)))
  (inner-p (- n 1)))

(p 561)
(p 1105)
(p 1729)
(p 2465)
(p 2821)
(p 6601)




(newline)
1.28
(define (miller-rabin-test n)
  (define (expmodn base exp)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let ((x (expmodn base (/ exp 2))))
             (if (and (not (or (= x 1)
                               (= x (- n 1))))
                      (= (remainder (square x) n) 1))        
                 0
                 (remainder (square x) n))))
          (else
            (remainder (* base (expmodn base (- exp 1))) n))))
  (define (test a)
    (cond ((> a (/ n 2)) true)       
          ((not (= (expmodn a (- n 1)) 1)) false)
          (else (test (+ 1 a)))))
  (test 1))

(miller-rabin-test 1729)
(miller-rabin-test 1019)
(miller-rabin-test 5)
(miller-rabin-test 6)
