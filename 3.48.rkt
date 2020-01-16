#lang sicp
;; REVISIT -- test with parallel-execute and atomic test-and-set! implementations

;; exchanges
(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let* ((serializer1 (account1 'serializer))
         (serializer2 (account2 'serializer))
         (exchanger (if (< (account1 'number) (account2 'number))  ;; -+
                        (serializer2 (serializer1 exchange))       ;;   +-- new 
                        (serializer1 (serializer2 exchange)))))    ;; -+
    (exchanger account1 account2)))                                ;; updated

#|
In the exchange problem, numbering the accounts and attempting to always acquire
the smaller-numbered account first (or greater-numbered first, it doesn't matter)
works because exchanges require us to acquire two known mutexes, and by ensuring
that every process that needs the same pair tries to acquire the same one first,
we eliminate the possibility that one process has acquired one of the two while
the second process has acquired the other.
|#


;; accounts
(define (make-account-and-serializer balance account-number)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'number) account-number)  ;; <-- new
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (deposit account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (withdraw account amount)
  (let ((s (account 'serializer))
        (d (account 'withdraw)))
    ((s d) amount)))


;; serializers
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
  (if (car cell) true (begin (set-car! cell true) false)))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))




