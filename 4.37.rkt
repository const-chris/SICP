#lang sicp
(#%require (file "./4.36.rkt"))

;; original
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

;; Ben's method
(define (bens-pythagorean-triple-between low high)
 (let ((i (an-integer-between low high))
       (hsq (* high high)))
  (let ((j (an-integer-between i high)))
   (let ((ksq (+ (* i i) (* j j))))
    (require (>= hsq ksq))
    (let ((k (sqrt ksq)))
     (require (integer? k))
     (list i j k))))))

#|
Ben is correct that his method is more efficient.

Examining the original algorithm, we can see that for each of i, j, and k, there are O(n) possibilities that must be examined,
where n = high - low. That means that the algorithm as a whole must explore O(n^3) possibilities.

Examining Ben's algorithm, we see immediately that k is completely determined by the choice of i and j. For each of i and j,
there are still O(n) possibilities, but for each pair (i, j), there is exactly one k to explore. Ben's algorithm, therefore,
must explore O(n^2) possibilities.
|#

;#| test
(define (time algorithm . args)
  (let ((start-time (runtime)))
    (apply algorithm args)
    (display (- (runtime) start-time))
    (newline)))

(define (compare-algorithms low high)
  (display (string-append "\ntime to find first pythagorean triple between " (number->string low) " and " (number->string high) "\n"))
  (display (make-string (+ 51 (string-length (number->string low)) (string-length (number->string high))) #\-))
  (newline)
  (display "original: ")
  (time a-pythagorean-triple-between low high)
  (display "Ben's:    ")
  (time bens-pythagorean-triple-between low high))

(compare-algorithms 2 20)
(compare-algorithms 11 100)
;|#
