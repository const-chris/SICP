#| Exercise 5.1: Design a register machine to compute factorials using the iterative algorithm specified by the following |#
#| procedure. Draw data-path and controller diagrams for this |#
#| machine. |#

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))


; Data Path:

        p<-m  ┌───────┐      p<-1
     ┌──Ⓧ────>│   p   │<─────Ⓧ─────┐
     │        └───────┘            │
     │            │                v
  ───────         │              ────
 ╱   *   ╲        │        ┌─────╲1 ╱───┐
╱─────────╲       │        │      ╲╱    │
  ^     ^         │        │            │
  │     │         │        Ⓧ c<-1       │
  │     └─────────┘        │            │
  │                        │            │
  │           ┌───────┐    │            V
  └───────────│   c   │<───┘       ╲─────────╱
              └───────┘             ╲   +   ╱
               │     ^               ╲─────╱
               v     │     c<-a         │
┌───────┐     .⌒.    └─────Ⓧ────────────┘
│   n   │───>( > )
└───────┘     `-'


; Controller:

               ┌───────┐       ┌───────┐
        ┌──────│  p<-1 │<──────│  c<-1 │<─────start
        │      └───────┘       └───────┘
        ^
       ╱ ╲
      ╱   ╲
 ┌──〈  >  〉───no────┐
 │    ╲   ╱           │
 │     ╲ ╱        ┌───────┐
 │      V         │  p<-m │
 │      │         └───────┘
 │      │             │
 │     yes            │
 │      │             │
 │      │             │
 │      v             │
 │    done            v
 │                ┌───────┐
 └────────────────│  c<-a │
                  └───────┘

