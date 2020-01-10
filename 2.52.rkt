#lang racket
(require "2.45.rkt" "2.46.rkt" "2.49.rkt" "2.50.rkt" "2.51.rkt" "picture-language.rkt")

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

; a)
; In 2.49.rkt (smile)

; b)
(define (corner-split-b painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

(define (square-limit-b painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split-b painter n))))


; c)
(define (square-limit-c painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))


; ((square-limit wave 3) window)
; ((square-limit-b wave 3) window)
((square-limit-c spiral 4) window)
  