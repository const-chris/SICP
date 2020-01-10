#lang racket
(require "2.46.rkt")
(provide make-segment start-segment end-segment)


(define (make-segment v1 v2) (list v1 v2))

(define (start-segment s) (car s))
(define (end-segment s) (cadr s))
