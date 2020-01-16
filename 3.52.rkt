#lang sicp
(#%require (file "stream-utils.rkt"))


(define sum 0)

;; sum = 0
(display "1. ")
sum



(define (accum x) (set! sum (+ x sum)) sum)

;; sum = 0
(display "2. ")
sum



(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))

;; sum = 1
(display "3. ")
sum



(define y (stream-filter even? seq))

;; sum = 6
(display "4. ")
sum



(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

;; sum = 10
(display "5. ")
sum



(newline)
(stream-ref y 7)
#|
with memo:
  seq = (1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,..)
  y   = (6,10,28,36,66,78,120,136,..)
  should print: 136

w/o memo:  162
  values (other than the car) need to be recomputed:
  after defining seq, its car is set to 1, and the sum is 1.
  after defining y, its car is 6, and the sum is 6.
  after defining z, its car is *15*, and the sum is *15*.

  When we begin calculating (stream-ref y 7):
  (car y) = 6
  sum = 15
  (stream-cdr seq) = (stream-map accum (stream-enumerate-interval 4 20))
  seq = (1,3,6,19,24,30,37,45,54,64,75,87,100,114,129,145,162,..)
  y   = (6,24,30,54,64,100,114,162,..)
  should print: 162
|#

(display-stream z)
#|
with memo:
  seq = (1,3,6,10,15,21,28,36,45,55,66,78,91,105,120,136,153,171,190,210)
  z   = (10,15,45,55,105,120,190,210)
  should print: 10 15 45 55 105 120 190 210

w/o memo: -
  sum = 162
  (car z) = 15
  (stream-cdr seq) = (stream-map accum (stream-enumerate-interval 5 20))
  seq = (1,8,11,15,167,173,180,197,207,218,230,243,257,272,288,305,323,342,362)
  z   = (15,180,230,305)
  should print: 15 180 230 305
|#