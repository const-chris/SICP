#lang sicp

;; from ex. 3.3
(define (make-account balance password)
  (define (validate p)
    (eq? p password))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (incorrect-password _)
    "Incorrect password")
  (define (dispatch p m) 
    (cond ((eq? m 'valid?) (validate p))
          ((not (eq? p password)) incorrect-password)
          ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  dispatch)



(define (make-joint account password new-password)
  (if (account password 'valid?)
      (lambda (p m)
        (cond ((not (eq? p new-password)) (account "" '_))
              ((eq? m 'withdraw) (account password 'withdraw))
              ((eq? m 'deposit) (account password 'deposit))
              (else (lambda (_)
                      (string-append "Unknown request: "
                                     (symbol->string m))))))
      (error "Incorrect password -- MAKE-JOINT" password)))




;; tests
(newline)

(display "(define peter-acc (make-account 100 'open-sesame))\n\n")
(define peter-acc (make-account 100 'open-sesame))

(display "(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))\n\n\n")
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))


(display "((paul-acc 'rosebud 'withdraw) 50)     -> ")
((paul-acc 'rosebud 'withdraw) 50)

(display "((paul-acc 'rosebutt 'withdraw) 50)    -> ")
((paul-acc 'rosebutt 'withdraw) 50)

(display "((paul-acc 'rosebud 'withdrawal) 50)   -> ")
((paul-acc 'rosebud 'withdrawal) 50)

(display "(define mary-acc
  (make-joint peter-acc 'open-says-me 'rosebud)) -> \n")
(define mary-acc
  (make-joint peter-acc 'open-says-me 'rosebud))



(newline)
