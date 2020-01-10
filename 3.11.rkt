#lang racket
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
           (error "Unknown request: MAKE-ACCOUNT"
                  m))))
  dispatch)


(define acc (make-account 50))
#|
                                                    parameters: balance
                                                    body: (define (withdraw amount) ...)
                                                          (define (deposit amount) ...)
                                                          (define (dispatch m) ...)
                                                          dispatch
                                                            ^
                                                            ║
                                                            ║
                                                            @=@═══╗
                                                             ^    ║
                                                             ║    ║
                                                             ║    v
          ╔══════════════════════════════════════════════════╬══════╗
          ║                                                  ║      ║
          ║ make-account:════════════════════════════════════╝      ║
global ══>║                                                         ║
env       ║ acc:══╗                                                 ║
          ║       ║                                                 ║
          ╚═══════╬═════════════════════════════════════════════════╝
                  ║          ^                               
                  ║          ║                 
                  ║          ║                                    
                  ║        ╔══════════════════════╗                                    
                  ║        ║                      ║
                  ║  E1 ══>║  balance: 50         ║<════════════════════╗                     
                  ║        ║                      ║                     ║
                  ║        ║  withdraw:════════════════════════════╗    ║   
                  ║        ║                      ║                ║    ║
                  ║        ║  deposit:══════╗     ║                ║    ║
                  ║        ║                ║     ║                v    ║
                  ║ ╔══════╬═ dispatch:     ║     ║               @═@═══╝  
                  ║ ║      ║                ║     ║               ║
                  ║ ║      ╚════════════════╬═════╝               ║
                  ║ ║        ^              ║    ^                v
                  v v        ║              ║    ║              parameters: amount
                  @═@════════╝              v    ║              body: (if (>= balance amount)
                  ║                        @═@═══╝                        (begin (set! balance (- balance amount))
                  ║                        ║                                     balance)
                  ║                        ║                              "Insufficient funds"
                  ║                        v
                  ║                      parameters: amount
                  ║                      body: (set! balance (+ balance amount))
                  ║                            balance
                  v
           parameters: m
           body: (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m)))

|#


((acc 'deposit) 40)
#|
                                                    parameters: balance
                                                    body: (define (withdraw amount) ...)
                                                          (define (deposit amount) ...)
                                                          (define (dispatch m) ...)
                                                          dispatch
                                                            ^
                                                            ║
                                                            ║
                                                            @=@═══╗
                                                             ^    ║
                                                             ║    ║
                                                             ║    v
          ╔══════════════════════════════════════════════════╬══════╗
          ║                                                  ║      ║
          ║ make-account:════════════════════════════════════╝      ║
global ══>║                                                         ║
env       ║ acc:══╗                                                 ║
          ║       ║                                                 ║
          ╚═══════╬═════════════════════════════════════════════════╝
                  ║          ^                               
                  ║          ║                 ╔═══════════════════╗
                  ║          ║                 ║                   ║
                  ║          ║                 v                   ║
                  ║        ╔═╩════════════════════╗             ╔══╩═════════════╗
                  ║  E1 ══>║  balance: 50         ║       E2 ══>║  m: 'deposit   ║ 
                  ║        ║  withdraw: ...       ║             ╚════════════════╝
                  ║        ║                      ║             call to (acc 'deposit)
                  ║        ║  deposit:══════╗     ║    
                  ║        ║                ║     ║<═══════════════╗
                  ║ ╔══════╬═ dispatch:     ║     ║                ║ 
                  ║ ║      ║                ║     ║             ╔══╩═════════════╗
                  ║ ║      ╚════════════════╬═════╝       E3 ══>║  amount: 40    ║    
                  ║ ║        ^              ║    ^              ╚════════════════╝
                  v v        ║              ║    ║              call to (deposit 40)
                  @═@════════╝              v    ║
                  ║                        @═@═══╝
                  ║                        ║
                  ║                        ║
                  ║                        v
                  ║                      parameters: amount
                  ║                      body: (set! balance (+ balance amount))
                  ║                            balance
                  v
           parameters: m
           body: (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m)))

|#

((acc 'withdraw) 60)
#|
                                                    parameters: balance
                                                    body: (define (withdraw amount) ...)
                                                          (define (deposit amount) ...)
                                                          (define (dispatch m) ...)
                                                          dispatch
                                                            ^
                                                            ║
                                                            ║
                                                            @=@═══╗
                                                             ^    ║
                                                             ║    ║
                                                             ║    v
          ╔══════════════════════════════════════════════════╬══════╗
          ║                                                  ║      ║
          ║ make-account:════════════════════════════════════╝      ║
global ══>║                                                         ║
env       ║ acc:══╗                                                 ║
          ║       ║                                                 ║
          ╚═══════╬═════════════════════════════════════════════════╝
                  ║          ^                               
                  ║          ║                 ╔═══════════════════╗
                  ║          ║                 ║                   ║
                  ║          ║                 v                   ║
                  ║        ╔═╩════════════════════╗             ╔══╩═════════════╗
                  ║  E1 ══>║  balance: 90         ║       E4 ══>║  m: 'withdraw  ║
                  ║        ║                      ║             ╚════════════════╝
                  ║        ║  withdraw:═════╗     ║             call to (acc 'deposit)
                  ║        ║                ║     ║             
                  ║        ║  deposit: ...  ║     ║    
                  ║        ║                ║     ║<═══════════════╗
                  ║ ╔══════╬═ dispatch:     ║     ║                ║ 
                  ║ ║      ║                ║     ║             ╔══╩═════════════╗
                  ║ ║      ╚════════════════╬═════╝       E5 ══>║  amount: 60    ║    
                  ║ ║        ^              ║    ^              ╚════════════════╝
                  v v        ║              ║    ║              call to (withdraw 60)
                  @═@════════╝              v    ║
                  ║                        @═@═══╝
                  ║                        ║
                  ║                        ║
                  ║                        v
                  ║                      parameters: amount
                  ║                      body: (if (>= balance amount)
                  ║                                (begin (set! balance (- balance amount))
                  ║                                       balance)
                  ║                                "Insufficient funds")
                  v
           parameters: m
           body: (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m)))

|#


(define acc2 (make-account 100))
#|
                                                    parameters: balance
                                                    body: (define (withdraw amount) ...)
                                                          (define (deposit amount) ...)
                                                          (define (dispatch m) ...)
                                                          dispatch
                                                            ^
                                                            ║
                                                            ║
                                                            @=@═══╗
                                                             ^    ║
                                                             ║    ║
                                                             ║    v
          ╔══════════════════════════════════════════════════╬════════════════════════════════════════════════════════════════════════════════════════╗
          ║                                                  ║                                                                                        ║
          ║ make-account:════════════════════════════════════╝                                                                                        ║
global ══>║                                                                                                                                           ║
env       ║ acc:══╗                                                            acc2:═══════════════════════════════════════════════════╗              ║
          ║       ║                                                                                                                    ║              ║
          ╚═══════╬════════════════════════════════════════════════════════════════════════════════════════════════════════════════════║══════════════╝
                  ║          ^                                                                                                         ║          ^
                  ║          ║                                                                                                         ║          ║
                  ║          ║                                                                                                         ║          ║
                  ║        ╔══════════════════════╗                                                                                    ║        ╔═╩════════════════════╗  
                  ║        ║                      ║                                                                                    ║        ║                      ║ 
                  ║  E1 ══>║  balance: 30         ║<════════════════╗                                                                  ║  E6 ══>║  balance: 100        ║<════════════════╗ 
                  ║        ║                      ║                 ║                                                                  ║        ║                      ║                 ║ 
                  ║        ║  withdraw:════════════════════════╗    ║                                                                  ║        ║  withdraw:═══════════╬════════════╗    ║
                  ║        ║                      ║            ║    ║                                                                  ║        ║                      ║            ║    ║
                  ║        ║  deposit:══════╗     ║            ║    ║                                                                  ║        ║  deposit:══════╗     ║            ║    ║
                  ║        ║                ║     ║            v    ║                                                                  ║        ║                ║     ║            v    ║
                  ║ ╔══════╬═ dispatch:     ║     ║           @═@═══╝                                                                  ║ ╔══════╬═ dispatch:     ║     ║           @═@═══╝  
                  ║ ║      ║                ║     ║           ║                                                                        ║ ║      ║                ║     ║           ║
                  ║ ║      ╚════════════════╬═════╝           ║                                                                        ║ ║      ╚════════════════╬═════╝           ║
                  ║ ║        ^              ║   ^             v                                                                        ║ ║        ^              ║   ^             ║
                  v v        ║              ║   ║           parameters: amount                                                         v v        ║              ║   ║             ║
                  @═@════════╝              v   ║           body: (if (>= balance amount)                                              @═@════════╝              v   ║             ║
                  ║                        @═@══╝                     (begin (set! balance (- balance amount)) <══════════╗            ║                        @═@══╝             ║
                  ║                        ║                                 balance)                                     ║            ║                        ║                  ║
                  ║                        ║                          "Insufficient funds"                                ╚════════════╬════════════════════════╬══════════════════╝
                  ║                        v                                                                                           ║                        ║
                  ║                      parameters: amount                                                                            ║                        ║
                  ║                      body: (set! balance (+ balance amount)) <═════════════════════════════════════════════════════╬════════════════════════╝
                  ║                            balance                                                                                 ║
                  v                                                                                                                    ║
           parameters: m                                                                                                               ║
           body: (cond ((eq? m 'withdraw) withdraw) <══════════════════════════════════════════════════════════════════════════════════╝
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request: MAKE-ACCOUNT" m)))

|#


#|
Local state is maintained in a separate environment for each account. Code (implementation dependent) and the global environment are shared.
|#