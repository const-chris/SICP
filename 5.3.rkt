#| Exercise 5.3: Design a machine to compute square roots using Newton’s method, as described in Section 1.1.7: |#

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

#| Begin by assuming that good-enough? and improve operations are available as primitives. Then show how to expand these in terms of arithmetic operations. Describe each version of the sqrt machine design by drawing a data-path diagram and writing a controller definition in the register machine language. |#

(controller
  sqrt-loop
    (assign x (op read))
    (assign g (const 1.0))
  test-g
    (test (op good-enough?) (reg g) (reg x))
    (branch (label sqrt-done))
    (assign g (op improve) (reg g) (reg x))
  sqrt-done
    (perform (op print) (reg g))
    (goto (label sqrt-loop)))

                          ╲────────╱
                           ╲ read ╱
                            ──────
                               │
                               Ⓧ x<-rd
                               │
                               V
                           ┌───────┐
      ┌────────────────────│   x   │────────────┐
      │                    └───────┘            │
      │  ┌─────────────────────┐                │
      │  │                     │                │
      V  V                     │                │
   ╲────────╱                  │                │
    ╲ imp  ╱                   │                V
     ──────                    │              .───.
       │                   ┌───────┐         ╱     ╲
       └─────────Ⓧ ───────>│   g   │───────>│ g-e?  │
               g<-imp      └───────┘         ╲     ╱
                             ^   │            .___.
                             │   │
                             ^   └──────────────┐
                            ╱1╲                 |
                           ╱___╲           ╲─────────╱
                                            ╲ print ╱
                                             ───────
                                                ^
                                                │
                                                Ⓧ P
                                                │
(controller
  sqrt-loop
    (assign x (op read))
    (assign g (const 1.0))
  good-enough?
    (assign t (op *) (reg g) (reg g))
    (assign t (op -) (reg t) (reg x))
    (assign t (op abs) (reg t))
    (test (op <) (reg g) (const 0.001))
    (branch (label sqrt-done))
  improve
    (assign t (op /) (reg x) (reg g))
  average
    (assign t (op +) (reg g) (reg t))
    (assign g (op /) (reg t) (const 2.0))
  sqrt-done
    (perform (op print) (reg g))
    (goto (label sqrt-loop)))

#| additional data-path for good-enough? |#
       ┌─────────────────────────┐
       │          ┌────────┐     │
       │          │        V     V
  ┌─────────┐     │      ╲─────────╱
  │    x    │     │       ╲   -   ╱
  └─────────┘     │        ───────
    ┌─────────────┘  t<-sub   │
    │  ┌───────────────Ⓧ ─────┘
    │  │   ┌──────────────────┐
    │  V   │                  V
  ┌─────────┐            ╲─────────╱
  │    t    │<────┐       ╲  abs  ╱
  └─────────┘     │        ───────
       ^          └────Ⓧ ─────┘
       │             t<-abs
       Ⓧ t<-mul
       │
    ───────
   ╱   *   ╲
  ╱─────────╲
    ^     ^
    │     │
  ┌─────────┐
  │    g    │
  └─────────┘
       │
       V
     .---.
    |  <  |
     .___.
       ^
       │
       ^
      ╱ ╲
     ╱   ╲
    ╱0.001╲
   ╱_______╲


#| additional data-path for improve |#
┌───────────────────────────┐   ┌──────┐
│      ┌──────────────────┐ │   │ ┌──┐ │
│      │                  V V   V V  │ │
│ ┌─────────┐            ╲─────────╱ │ │
│ │    x    │             ╲   /   ╱  │ │
│ └─────────┘              ───────   │ │
│                  t<-div   │   |    │ │
│  ┌─────────────────Ⓧ ─────┘   └────────┐
│  │   ┌─────────────────────────────┘ │ │
│  │   │   ┌───────────────┐     ┌───┐ │ │
│  V   │   │               V     V   │ │ │
│ ┌─────────┐            ╲─────────╱ │ │ │
│ │    t    │<────┐       ╲   +   ╱  │ │ │
│ └─────────┘     │        ───────   │ │ │
│                 └────Ⓧ ─────┘      │ │ │
│                    t<-add          │ │ │
│ ┌─────────┐                        │ │ │
└─│    g    │────────────────────────┘ │ │
  └─────────┘                          │ │
    ^     └────────────────────────────┘ │
    └──────────────────Ⓧ ────────────────┘
                     g<-div
