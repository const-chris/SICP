#lang racket
"
Recursive version:

                    parameters: n
                    body: (if (= n 1)
                              1
                              (* n (factorial (- n 1))))
                         ^
                         ║
                         ║
                       ╔═╩═╦═══╗
                       ║ o ║ o ╠═══╗
                       ╚═══╩═══╝   ║
                          ^        ║
                          ║        ║
                          ║        ║
                          ║        v
               ╔══════════╩══════════════════════════════════════════════════════╗
global env ═══>║      factorial                                                  ║
               ╚═════════════════════════════════════════════════════════════════╝
    (factorial 6)  ^                               ^                        ^
                   ║                               ║                        ║
                   ║                               ║                        ║
                   ║                               ║                        ║
               ╔═══╩═══╗                       ╔═══╩═══╗                ╔═══╩═══╗
        E1 ═══>║ n: 6  ║                E2 ═══>║ n: 5  ║   ...   E6 ═══>║ n: 1  ║
               ╚═══════╝                       ╚═══════╝                ╚═══════╝
              (if (= n 1)                      ``                       ``
                  1
                  (* n (factorial (- n 1))))







Iterative version:

                                                                           parameters: product, counter, max-counter
                                                                           body: (if (> counter max-count)
                                                                                     product
                                                                                     (fact-iter (* counter product)
                    parameters: n                                                               (+ counter 1)
                    body: (fact-iter 1 1 n)                                                     max-count)))
                         ^                                                      ^                                                                
                         ║                                                      ║
                         ║                                                      ║
                       ╔═╩═╦═══╗                                              ╔═╩═╦═══╗
                       ║ o ║ o ╠═══╗                                          ║ o ║ o ╠═══╗
                       ╚═══╩═══╝   ║                                          ╚═══╩═══╝   ║
                           ^       ║                                              ^       ║
                           ║       ║                                              ║       ║                  
                           ║       ║                                              ║       ║
                           ║       v                                              ║       v
               ╔═══════════╩══════════════════════════════════════════════════════╩═════════════════════════════════════════════════════════════════╗
global env ═══>║       factorial                                              fact-iter                                                             ║
               ╚════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝
    (factorial 6)  ^                                     ^                                          ^                                          ^
                   ║                                     ║                                          ║                                          ║
                   ║                                     ║                                          ║                                          ║
                   ║                                     ║                                          ║                                          ║
               ╔═══╩═══╗                       ╔═══════════════════╗                      ╔═════════╩═════════╗                      ╔═════════╩═════════╗
        E1 ═══>║ n: 6  ║                E2 ═══>║ product: 1        ║               E3 ═══>║ product: 1        ║      ...      E8 ═══>║ product: 720      ║
               ╚═══════╝                       ║ counter: 1        ║                      ║ counter: 2        ║                      ║ counter: 7        ║
              (fact-iter 1 1 n)                ║ max-counter: 6    ║                      ║ max-counter: 6    ║                      ║ max-counter: 6    ║
                                               ╚═══════════════════╝                      ╚═══════════════════╝                      ╚═══════════════════╝
                                              (if (> counter max-count)                   ``                                         ``
                                                  product
                                                  (fact-iter (* counter product)
                                                             (+ counter 1)
                                                             max-count)))

"