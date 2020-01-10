#lang sicp

; 1.34
(define (f g)
  (g 2))

; (f f)
; Tries to call (f 2) -> (2 2), and 2 is not a procedure
