#lang sicp

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else
           (error "Undefined operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value) z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value) z)




(define x (cons 1 2))
#|
                   parameters: x, y
                   body: (define (set-x! v) (set! x v))
                         (define (set-y! v) (set! y v))
                         (define (dispatch m)
                           (cond ((eq? m 'car) x)
                                 ((eq? m 'cdr) y)
                                 ((eq? m 'set-car!) set-x!)
                                 ((eq? m 'set-cdr!) set-y!)
                                  (else
                                  (error "Undefined operation: CONS" m))))
                         dispatch
                         ^
                         ║
                         ║
                         @=@════╗
                          ^     ║
                          ║     v
                ╔═════════╬═══════════╗
                ║         ║           ║
global env ═══> ║ cons: ══╝           ║
                ║                     ║
                ║ x: ═══╗             ║
                ║       ║             ║
                ╚═══════╬═════════════╝
                        ║         ^
                        ║         ║
                        ║         ║
                        ║     ╔═══╩═════════╗
                        ║     ║ x: 1        ║ <═══ E1
                        ║     ║ y: 2        ║
                        ║     ║ set-x!: ... ║
                        ║     ║ set-y!: ... ║
                        ║ ╔════ dispatch    ║
                        ║ ║   ╚═════════════╝
                        ║ ║       ^
                        ║ ║       ║
                        v v       ║
                        @=@═══════╝
                        ║
                        ║
                        v
                     parameters: m
                     body: (cond ((eq? m 'car) x)
                                 ((eq? m 'cdr) y)
                                 ((eq? m 'set-car!) set-x!)
                                 ((eq? m 'set-cdr!) set-y!)
                                  (else
                                  (error "Undefined operation: CONS" m)))
|#

(define z (cons x x))
#|
                   parameters: x, y
                   body: (define (set-x! v) (set! x v))
                         (define (set-y! v) (set! y v))
                         (define (dispatch m)
                           (cond ((eq? m 'car) x)
                                 ((eq? m 'cdr) y)
                                 ((eq? m 'set-car!) set-x!)
                                 ((eq? m 'set-cdr!) set-y!)
                                  (else
                                  (error "Undefined operation: CONS" m))))
                         dispatch
                         ^
                         ║
                         ║
                         @=@════╗
                          ^     ║                                      ╔═════════════╗
                          ║     v                                      ║ x: x        ║
                ╔═════════╬═══════════╗                                ║ y: x        ║ <═══ E2
                ║         ║           ║ <══════════════════════════════╣ set-x!: ... ║
global env ═══> ║ cons: ══╝           ║                                ║ set-y!: ... ║
                ║                     ║                        ╔═══════╣ dispatch    ║
                ║ x: ═══╗      z: ═══════════════════════════╗ ║       ╚═════════════╝
                ║       ║             ║                      ║ ║           ^
                ╚═══════╬═════════════╝                      ║ ║           ║
                        ║         ^                          v v           ║
                        ║         ║                          @=@═══════════╝
                        ║         ║                          ║
                        ║     ╔═══╩═════════╗                ║
                        ║     ║ x: 1        ║ <═══ E1        ║
                        ║     ║ y: 2        ║                ║
                        ║     ║ set-x!: ... ║                ║
                        ║     ║ set-y!: ... ║                ║
                        ║ ╔════ dispatch    ║                ║
                        ║ ║   ╚═════════════╝       ╔════════╝
                        ║ ║       ^                 ║
                        ║ ║       ║                 ║
                        v v       ║                 ║
                        @=@═══════╝                 ║
                        ║             ╔═════════════╝
                        ║             ║
                        v             ║
                     parameters: m    v
                     body: (cond ((eq? m 'car) x)
                                 ((eq? m 'cdr) y)
                                 ((eq? m 'set-car!) set-x!)
                                 ((eq? m 'set-cdr!) set-y!)
                                  (else
                                  (error "Undefined operation: CONS" m)))
|#

(set-car! (cdr z) 17)
#|
                   parameters: x, y          parameters: z                 parameters: z, new-value
                   body: ...                 body: (z 'cdr)                body: ((z 'set-car!) new-value) z
                         ^                        ^                             ^
                         ║                        ║                             ║
                         ║                        ║                             ║
                         @=@════╗                 @=@════╗                      @=@════╗
                          ^     ║                  ^     ║                       ^     ║
                          ║     v                  ║     v                       ║     v
                ╔═════════╬═══════════-------------|-----------------------------|---------------------------------------------------------------------------------------------------------------╗
                ║         ║                        |                             |                                                                                                               ║
global env ═══> ║ cons: ══╝                 cdr: --+                 set-car!: --+                                                                                                               ║
                ║                                                                                                                                                                                ║
                ║ x: ═══╗                         z: ═══+                                                                                                                                        |
                ║       ║                               |                                                                                                                                        ║
                ╚═══════╬════════════-------------------|----------------------------------------------------------------------------------------------------------------------------------------╝
                        ║         ^                     |            ^                                      ^                              ^
                        ║         ║                     |            |                                      |                              |
                        ║         ║                     |            |                                      |                              |
                        ║     ╔═══╩═════════╗           |         ╔═════════════╗                       +------------+                 +---------------+
                        ║     ║ x: 1        ║ <═══ E1   |         ║ x: x        ║                       | z: z       | <--- E3         | z: x          | <--- E5
                        ║     ║ y: 2        ║           |         ║ y: x        ║ <═══ E2               +------------+                 | new-value: 17 |
                        ║     ║ set-x!: ... ║           |         ╣ set-x!: ... ║                       call to cdr                    +---------------+
                        ║     ║ set-y!: ... ║           |         ║ set-y!: ... ║                                                      call to set-car!
                        ║ ╔════ dispatch    ║           | ╔═══════╣ dispatch    ║
                        ║ ║   ╚═════════════╝           ╗ ║       ╚═════════════╝
                        ║ ║       ^   ^   ^             | ║           ^      ^
                        ║ ║       ║   |   |             ║ ║           ║      |
                        v v       ║   |   |             v v           ║      |
                        @=@═══════╝   |   |             @=@═══════════╝   +------------+
                        |             |   |             |                 | m: 'cdr    | <--- E4
                        ║ +-----------|---|═════════════╝                 +------------+
                        ║ ║           |   |                               call to z                       +--------------+
                        | |           |   +---------------------------------------------------------------| m: 'set-car! | <--- E6
                        v v           |                                                                   +--------------+
                     parameters: m    +------------------------+                                          call to dispatch
                     body: (cond ((eq? m 'car) x)              |
                                 ((eq? m 'cdr) y)              |
                                 ((eq? m 'set-car!) set-x!)    |                                          +--------------+
                                 ((eq? m 'set-cdr!) set-y!)    +------------------------------------------| v: 17        | <--- E7
                                  (else                                                                   +--------------+
                                  (error "Undefined operation: CONS" m)))                                 call to set-x!
|#


(car x)
#|
                ╔---------------------------------------------------------------------------------╗
                ║                           car: ...                 set-car!: ...                ║
global env ═══> ║ cons: ...                 cdr: ...                 set-cdr!: ...                ║
                ║                                                                                 ║
                ║ x: ═══╗                         z: ═══+                                         |
                ║       ║                               |                                         ║
                ╚═══════╬════════════-------------------|-----------------------------------------╝
                        ║         ^                     |            ^                         ^
                        ║         ║                     |            |                         |
                        ║         ║                     |            |                         |
                        ║     ╔═══╩═════════╗           |         ╔═════════════╗            +---------------+
                        ║     ║ x: 17       ║ <═══ E1   |         ║ x: x        ║            | z: x          | <--- E8
                        ║     ║ y: 2        ║           |         ║ y: x        ║ <═══ E2    +---------------+
                        ║     ║ set-x!: ... ║           |         ╣ set-x!: ... ║            call to car
                        ║     ║ set-y!: ... ║           |         ║ set-y!: ... ║
                        ║ ╔════ dispatch    ║           | ╔═══════╣ dispatch    ║
                        ║ ║   ╚═════════════╝           ╗ ║       ╚═════════════╝
                        ║ ║       ^  ^                  | ║           ^
                        ║ ║       ║  |                  ║ ║           ║
                        v v       ║  |                  v v           ║
                        @=@═══════╝  |                  @=@═══════════╝
                        |            |                  |
                        ║ +----------|-----═════════════╝
                        ║ ║          |                                    +---------------+
                        | |          +------------------------------------| m: 'car       | <--- E9
                        v v                                               +---------------+
                     parameters: m                                        call to dispatch
                     body: (cond ((eq? m 'car) x)
                                 ((eq? m 'cdr) y)
                                 ((eq? m 'set-car!) set-x!)
                                 ((eq? m 'set-cdr!) set-y!)
                                  (else
                                  (error "Undefined operation: CONS" m)))
|#
