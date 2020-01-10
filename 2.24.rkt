#lang sicp


(list 1 (list 2 (list 3 4)))

The interpreter prints (1 (2 (3 4)))


The box and pointer diagram and the tree diagram
are different representations of the same idea:
namely, that (1 (2 (3 4))) is a triply nested structure
where each layer is a list of two items,
a number and the next layer:

box-and-pointer

                  ╔═══╦═══╗           ╔═══╦═══╗
 (1 (2 (3 4))) ══>║ * ║ * ╠══════════>║ * ║ / ║
                  ╚═╦═╩═══╝           ╚═╦═╩═══╝
                    ║                   ║ 
                    ║                   ║
                    ║                   ║
                    V                   V
                  ╔═══╗               ╔═══╦═══╗           ╔═══╦═══╗
                  ║ 1 ║  (2 (3 4)) ══>║ * ║ * ╠══════════>║ * ║ / ║
                  ╚═══╝               ╚═╦═╩═══╝           ╚═╦═╩═══╝
                                        ║                   ║ 
                                        ║                   ║
                                        ║                   ║
                                        V                   V
                                      ╔═══╗               ╔═══╦═══╗            ╔═══╦═══╗
                                      ║ 2 ║      (3 4) ══>║ * ║ * ╠═══════════>║ * ║ / ║
                                      ╚═══╝               ╚═╦═╩═══╝            ╚═╦═╩═══╝
                                                            ║                    ║
                                                            ║                    ║
                                                            ║                    ║
                                                            V                    V
                                                          ╔═══╗                ╔═══╗
                                                          ║ 3 ║                ║ 4 ║
                                                          ╚═══╝                ╚═══╝

tree

              (1 (2 (3 4)))
                  /   \
                 1     (2 (3 4))
                         /   \
                        2     (3 4)
                              /   \
                             3     4

