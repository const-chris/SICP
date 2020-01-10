#lang sicp
(newline)




(list 'a 'b 'c)
; should be (a b c)

(list (list 'george))
; should be ((george))

(cdr '((x1 x2) (y1 y2)))
; should be ((y1 y2))

(cadr '((x1 x2) (y1 y2)))
; should be (y1 y2)

(pair? (car '(a short list)))
; should be #f

(memq 'red '((red shoes) (blue socks)))
; should be #f

(memq 'red '(red shoes blue socks))
; should be (red shoes blue socks)




(newline)
