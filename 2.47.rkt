#lang racket
(provide make-frame origin-frame edge1-frame edge2-frame)


(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))




(define (make-frame-alt origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (frame-alt-origin f) (car f))
(define (frame-alt-edge1 f) (cadr f))
(define (frame-alt-edge2 f) (cddr f))