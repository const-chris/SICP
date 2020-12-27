#| Exercise 5.2: Use the register-machine language to describe |#
#| the iterative factorial machine of Exercise 5.1. |#

(controller
    (assign c (const 1))
    (assign p (const 1))
  test-c
    (test (op >) (reg c) (reg n))
    (branch (label fact-done))
    (assign p (op mul) (reg c) (reg p))
    (assign c (op add) (reg c) (const 1))
    (goto (label test-c))
  fact-done)

