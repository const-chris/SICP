#lang racket

;; a
(display "Peter -> Paul -> Mary: ")
(display (/ (- (+ 100 10) 20) 2))
(newline)

(display "Peter -> Mary -> Paul: ")
(display (- (/ (+ 100 10) 2) 20))
(newline)

(display "Paul -> Peter -> Mary: ")
(display (/ (+ (- 100 20) 10) 2))
(newline)

(display "Paul -> Mary -> Peter: ")
(display (+ (/ (- 100 20) 2) 10))
(newline)

(display "Mary -> Peter -> Paul: ")
(display (- (+ (/ 100 2) 10) 20))
(newline)

(display "Mary -> Paul -> Peter: ")
(display (+ (- (/ 100 2) 20) 10))
(newline)


;; b
#|

bank           Peter              Paul            Mary
100
           access: 100          access: 100      access: 100
           new val: 110         new val: 80      new val: 50
           set! balance: 110
110
                                                 set! balance: 50
50
                                set! balance: 80
80





100
           access: 100          access: 100      access: 100
           new val: 110         new val: 80      new val: 50
                                                 set! balance: 50
50
                                set! balance: 80
80
           set! balance: 110
110





100
                                access: 100      access: 100
                                new val: 80      new val: 50
                                                 set! balance: 50
50
                                set! balance: 80
80
           access: 80
           new val: 90
           set! balance: 90
90





100
                                access: 100      access: 100
                                new val: 80      new val: 50
                                set! balance: 80
80
                                                 set! balance: 50
50
           access: 50
           new val: 60
           set! balance: 60
60





100
          access: 100                           access: 100
          new val: 110                          new val: 50
          set! balance: 110
110
                                                 set! balance: 50
50
                               access: 50
                               new val: 30
                               set! balance: 30
30





100
           access: 100                           access: 100
           new val: 110                          new val: 50
                                                 set! balance: 50
50
           set! balance: 110
110
                                access: 110
                                new val: 55
                                set! balance: 55
55
|#
