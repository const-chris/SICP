#lang sicp
;; REVISIT -- test with implementation of parallel-execute

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin
        (set-car! cell true)
        false)))




;; a) semaphore in terms of mutexes:
(define (make-semaphore-a n)
  (let ((count 0)
        (lock (make-mutex)))   
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (lock 'acquire)
             (if (< count n)
                 (begin
                   (set! count (+ count 1))
                   (lock 'release))
                 (begin
                   (lock 'release)
                   (the-semaphore 'acquire))))  ;; retry
            ((eq? m 'release)
             (lock 'acquire)
             (set! count (- count 1))
             (lock 'release))))
    the-semaphore))




;; b) semaphore in terms of atomic test-and-set! operations:
(define (make-semaphore-b n)
  (let ((count 0)
        (lock (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (cond ((test-and-set! lock)
                    (the-semaphore 'acquire))   ;; retry
                   ((< count n)
                    (set! count (+ count 1))
                    (clear! lock))
                   (else
                    (clear! lock)
                    (the-semaphore 'acquire)))) ;; retry        
            ((eq? m 'release)
             (if (test-and-set! lock)
                 (the-semaphore 'release)       ;; retry
                 (begin
                   (set! count (- count 1))
                   (clear! lock))))))
    the-semaphore))





















