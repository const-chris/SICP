#lang sicp

; (define (make-mobile left right) (list left right))
; (define (make-branch length structure) (list length structure))


; d)
(define (make-mobile left right) (cons left right))
(define (make-branch length structure) (cons length structure))

; we need only change our cadrs to cdrs in the selectors to make everything work this way
(define (right-branch mobile) (cdr mobile))
(define (branch-structure mobile) (cdr mobile))




; a)
(define (left-branch mobile) (car mobile))
;(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
;(define (branch-structure branch) (cadr branch))




(define mobile1 (make-mobile (make-branch 5 10)
                             (make-branch 2.5 20)))

(define mobile2 (make-mobile (make-branch 10 mobile1)
                             (make-branch 15 20)))

;(left-branch mobile1)
;(branch-structure (right-branch mobile1))

;(left-branch mobile2)
;(branch-length (right-branch (branch-structure (left-branch mobile2))))




; b)
(define (total-weight mobile)             
  (cond ((not (pair? mobile)) mobile)
        (else (+ (total-weight (branch-structure (left-branch mobile)))
                 (total-weight (branch-structure (right-branch mobile)))))))

;(total-weight mobile1)
;(total-weight mobile2)




; c)
(define (balanced? mobile)
  (define (torque branch)
    (* (total-weight (branch-structure branch))
       (branch-length branch)))
  (or (not (pair? mobile))
      (and (= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (balanced? (branch-structure (left-branch mobile)))
           (balanced? (branch-structure (right-branch mobile))))))

;(balanced? mobile1)
;(balanced? mobile2)




