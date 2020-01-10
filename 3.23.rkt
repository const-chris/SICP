#lang sicp

(define (make-link prev-link value)
  (list (cons prev-link value)))

(define (rest-links link) (cdr link))
(define (prev-link link) (caar link))
(define (value link) (cdar link))

(define (set-rest-links! link items) (set-cdr! link items))
(define (set-prev-link! link item) (set-car! (car link) item))




(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (value (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (value (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (let ((new-pair (list (cons '() item))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-prev-link! (front-ptr deque) new-pair)
           (set-rest-links! new-pair (front-ptr deque))
           (set-front-ptr! deque new-pair)
           deque))))

(define (rear-insert-deque! deque item)
  (let ((new-pair (list (cons (rear-ptr deque) item))))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           deque)
          (else
           (set-rest-links! (rear-ptr deque) new-pair)
           (set-rear-ptr! deque new-pair)
           deque))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((= (length (car deque)) 1)
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else
         (set-front-ptr! deque (rest-links (front-ptr deque)))
         (set-prev-link! (front-ptr deque) '())
         deque)))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        ((= (length (car deque)) 1)
         (set-front-ptr! deque '())
         (set-rear-ptr! deque '())
         deque)
        (else
         (set-rear-ptr! deque (prev-link (rear-ptr deque)))
         (set-rest-links! (rear-ptr deque) '())
         deque)))


(define (make-deque) (cons '() '()))




;; tests
(newline)

(define (pp deque)
  (map cdr (front-ptr deque)))

(define q (make-deque))
(display "(pp q)                          = ")
(pp q)

(display "(pp (front-insert-deque! q 'a)) = ")
(pp (front-insert-deque! q 'a))

(display "(pp (front-insert-deque! q 'b)) = ")
(pp (front-insert-deque! q 'b))

(display "(pp (front-insert-deque! q 'c)) = ")
(pp (front-insert-deque! q 'c))

(display "(front-deque q)                 = ")
(front-deque q)

(display "(rear-deque q)                  = ")
(rear-deque q)

(display "(pp (rear-insert-deque! q 'c))  = ")
(pp (rear-insert-deque! q 'c))

(display "(pp (rear-insert-deque! q 'd))  = ")
(pp (rear-insert-deque! q 'd))

(display "(pp (rear-delete-deque! q))     = ")
(pp (rear-delete-deque! q))

(display "(pp (front-delete-deque! q))    = ")
(pp (front-delete-deque! q))

(display "(pp (front-delete-deque! q))    = ")
(pp (front-delete-deque! q))

(display "(pp (rear-delete-deque! q))     = ")
(pp (rear-delete-deque! q))

(display "(pp (rear-delete-deque! q))     = ")
(pp (rear-delete-deque! q))

(display "(pp (front-insert-deque! q 'a)) = ")
(pp (front-insert-deque! q 'a))

(display "(pp (front-delete-deque! q))    = ")
(pp (front-delete-deque! q))

(display "(pp (front-insert-deque! q 'a)) = ")
(pp (front-insert-deque! q 'a))

(display "(pp (rear-delete-deque! q))     = ")
(pp (rear-delete-deque! q))

(display "(pp (rear-insert-deque! q 'a))  = ")
(pp (rear-insert-deque! q 'a))

(display "(pp (front-delete-deque! q))    = ")
(pp (front-delete-deque! q))

(display "(pp (rear-insert-deque! q 'a))  = ")
(pp (rear-insert-deque! q 'a))

(display "(pp (rear-delete-deque! q))     = ")
(pp (rear-delete-deque! q))


;; (pp (rear-delete-deque! q))
;; (pp (front-delete-deque! q))
;; (rear-deque q)
;; (front-deque q)




(newline)
