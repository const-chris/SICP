#lang scheme

; 1.20

; applicative-order:

(gcd 206 400)
(if (= 0 40)
    206
    (gcd 40 (remainder 206 40)))  ; 1

(gcd 40 6)
(if (= 0 6)
    40
    (gcd 6 (remainder 40 6)))     ; 2

(gcd 6 4)
(if (= 0 4)
    6
    (gcd 4 (remainder 6 4)))      ; 3

(gcd 4 2)
(if (= 0 2)
    4
    (gcd 2 (remainder 4 2)))      ; 4

(gcd 2 0)
(if (= 0 0)
    2
    (gcd 0 (remainder 2 0)))        ; never gets here

2
; remainder is called 4 times

; normal-order:
(gcd 206 400)
(if (= 0 40)
    206
    (gcd 40 (remainder 206 40)))

(gcd 40 (remainder 206 40))
(if (= 0 (remainder 206 40))                                                                                                       ; 1 (in predicate of if statement)
    40
    (gcd (remainder 206 40)
         (remainder 40 (remainder 206 40))))    

(gcd (remainder 206 40)
     (remainder 40 (remainder 206 40)))
(if (= 0 (remainder 40 (remainder 206 40)))                                                                                        ; 2, 3 (in predicate of if statement)
    (remainder 206 40)
    (gcd (remainder 40 (remainder 206 40))
         (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(if (= 0 (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))                                                         ; 4-7 (in predicate of if statement)
    (remainder 40 (remainder 206 40))
    (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
         (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(if (= 0 (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))           ; 8-14 (in predicate of if statement)
    (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))                                                               ; 15-18 (in then-clause)
    (gcd (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))            ; never gets here
         (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))                                               
                    (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

2
; remainder is called 18 times