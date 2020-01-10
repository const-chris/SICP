#lang racket
(require "2.42.2.rkt" "chapter2.rkt")
(newline)


; Louis's version is much slower because he places the recursive call to queen-cols in the inner loop, so for each k,
; the interpreter makes n = board-size calls to queens-cols.
; The total number of calls to queens-cols using Louis's algorithm is:
;         board-size + board-size^2 + board-size^3 + ... + board-size^(board-size - 1)

; In the original algorithm, we make one call to queen-cols for each k.
; The total number of calls to queen-cols using the original algorithm is board-size.

; If the original algorithm takes time T, we would expect Louis's algorithm to take roughly T + T^2 + ... + T^(board-size - 1).


(define (louis-queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap (lambda (new-row)
                           (map (lambda (rest-of-queens)
                                  (adjoin-position new-row k rest-of-queens))
                                (queen-cols (- k 1))))
                         (enumerate-interval 1 board-size)))))
  (queen-cols board-size))


(define (time-queens proc n)
  (display n)
  (display ": ")
  (time (proc n) (proc n)))


(time-queens louis-queens 5)
(time-queens queens 5)

(time-queens louis-queens 6)
(time-queens queens 6)

(time-queens louis-queens 7)
(time-queens queens 7)

(time-queens louis-queens 8)
(time-queens queens 8)

; REVISIT -- FIGURE OUT TIME COMPLEXITY FOR LOUIS'S QUEENS

