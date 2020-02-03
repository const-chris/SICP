#lang sicp

#|
(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))


gets desugared to:
|#

(define (make-withdraw initial-amount)
  ((lambda (balance)
     (lambda (amount)
       (if (>= balance amount)
           (begin (set! balance (- balance amount))
                  balance)
           "Insufficient funds")))
   initial-amount))
#|
              ╔═════════════════════════╗
              ║                         ║
global env ══>║ make-withdraw: ══╗      ║
              ║                  ║      ║
              ╚══════════════════╣══════╝
                                 ║   ^
                                 v   ║
                                @═@══╝
                                ║
                                ║
                                v
                           parameters: initial-amount
                           body: ((lambda (balance)
                                   (lambda (amount)
                                     (if (>= balance amount)
                                         (begin (set! balance (- balance amount))
                                                balance)
                                         "Insufficient funds")))
                                 initial-amount)
|#


(define W1 (make-withdraw 100))
#|
          ╔════════════════════════════════════════════════════════╗
          ║                                                        ║
          ║ make-withdraw:══════════════════════════════════╗      ║
global -->║                                                 ║      ║
env       ║ W1:══╗                                          ║      ║
          ║      ║                                          ║      ║
          ╚══════╬══════════════════════════════════════════╬══════╝
                 ║          ^                               ║   ^
                 ║          ║                               ║   ║
                 ║          ║                               v   ║
                 ║        ╔═╩═══════════════════╗          @═@══╝
                 ║  E1 ══>║ initial-amount: 100 ║           ║
                 ║        ╚═════════════════════╝           ║
                 ║          ^                               v
                 ║          ║                        parameters: initial-amount
                 ║          ║                        body: ((lambda (balance) ...)
                 ║        ╔═╩═══════════════════╗
                 ║  E2 ══>║ balance: 100        ║
                 ║        ╚═════════════════════╝
                 ║         ^
                 v         ║
                @═@════════╝
                ║
                ║
                v
           parameters: amount
           body: (if (>= balance amount)
                     (begin (set! balance (- balance amount))
                            balance)
                     "Insufficient funds")
|#


(W1 50)
#|
          ╔════════════════════════════════════════════════════════╗
          ║ make-withdraw: ...                                     ║
global ══>║                                                        ║
env       ║ W1:══╗                                                 ║
          ╚══════╠═════════════════════════════════════════════════╝
                 ║          ^
                 ║          ║
                 ║          ║
                 ║        ╔═╩═══════════════════╗
                 ║  E1 ══>║ initial-amount: 100 ║
                 ║        ╚═════════════════════╝
                 ║          ^
                 ║          ║
                 ║          ║
                 ║        ╔═╩═══════════════════╗
                 ║  E2 ══>║ balance: 100        ║
                 ║        ╚═════════════════════╝
                 ║          ^  ^
                 ║          ║  ║
                 v          ║  ║
                @═@═════════╝  ╚══════════════╗
                ║                             ║
                ║                             ║
                v                             ║
           parameters: amount               ╔═╩══════════╗
           body: (if ...)             E3 ══>║ amount: 50 ║
                                            ╚════════════╝
|#


(define W2 (make-withdraw 100))
#|
          ╔══════════════════════════════════════════════════════════╗
          ║                                                          ║
          ║ make-withdraw: ...                                       ║
          ║                                                          ║
global -->║ W1:══╗                                                   ║
env       ║      ║                                                   ║
          ║ W2:══╬══════════════════════════════════╗                ║
          ║      ║                                  ║                ║
          ╚══════╬══════════════════════════════════╬════════════════╝
                 ║          ^                       ║          ^
                 ║          ║                       ║          ║
                 ║          ║                       ║          ║
                 ║        ╔═╩═══════════════════╗   ║        ╔═╩═══════════════════╗
                 ║  E1 ══>║ initial-amount: 100 ║   ║  E3 ══>║ initial-amount: 100 ║
                 ║        ╚═════════════════════╝   ║        ╚═════════════════════╝
                 ║          ^                       ║          ^
                 ║          ║                       ║          ║
                 ║          ║                       ║          ║
                 ║        ╔═╩═══════════════════╗   ║        ╔═╩═══════════════════╗
                 ║  E2 ══>║ balance: 50         ║   ║  E4 ══>║ balance: 100        ║
                 ║        ╚═════════════════════╝   ║        ╚═════════════════════╝
                 ║          ^                       ║          ^
                 ║          ║                       ║          ║
                 v          ║                       v          ║
                @═@═════════╝                      @═@═════════╝
                ║                                  ║
                ║   ╔══════════════════════════════╝
                ║   ║
                ║   ║
                v   v
           parameters: amount
           body: (if (>= ...) ...)
|#


#|
This version has the same behavior as the make-withdraw without the explicit let binding,
but it generates an extra frame for the desugared let binding every time it is called.
|#
