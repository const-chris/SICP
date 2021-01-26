; simulating (fact 3):

; instruction                                    continue             n          val              stack
; --------------------------------------------------------------------------------------------------------------------------------
(assign continue (label fact-done))                                   3                           []
(test (op =) (reg n) (const 1))                  fact-done
(save continue)
(save n)                                                                                          [fact-done]
(assign n (op -) (reg n) (const 1))                                                               [3, fact-done]
(assign continue (label after-fact))                                  2
(goto (label fact-loop))                         after-fact
(test (op =) (reg n) (const 1))
(save continue)
(save n)                                                                                          [after-fact, 3, fact-done]
(assign n (op -) (reg n) (const 1))                                                               [2, after-fact, 3, fact-done]
(goto (label fact-loop))                                              1
(test (op =) (reg n) (const 1))
(branch (label base-case))
(assign val (const 1))
(goto (reg continue))                                                            1
(restore n)
(restore continue)                                                    2                           [after-fact, 3, fact-done]
(assign val (op *) (reg n) (reg val))                                                             [3, fact-done]
(goto (reg continue))                                                            2
(restore n)
(restore continue)                                                    3                           [fact-done]
(assign val (op *) (reg n) (reg val))            fact-done                                        []
(goto reg continue)                                                              6


; simulating (fib 4):

; instruction                                    continue             n          val              stack
; --------------------------------------------------------------------------------------------------------------------------------
(assign continue (label fib-done))                                    4                           []
(test (op =) (reg n) (const 2))                  fib-done
(save continue)
(assign continue (label afterfib-n-1))                                                            [fib-done]
(save n)                                         afterfib-n-1
(assign n (op -) (reg n) (const 1))                                                               [4, fib-done]
(goto (label fib-loop))                                               3
(test (op =) (reg n) (const 2))
(save continue)
(assign continue (label afterfib-n-1))                                                            [afterfib-n-1, 4, fib-done]
(save n)
(assign n (op -) (reg n) (const 1))                                                               [3, afterfib-n-1, 4, fib-done]
(goto (label fib-loop))                                               2
(test (op =) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n))
(goto (reg continue))                                                            2
(restore n)
(assign n (op -) (reg n) (const 2))                                   3                           [afterfib-n-1, 4, fib-done]
(assign continue (label afterfib-n-2))                                1
(save val)                                       afterfib-n-2
(goto (label fib-loop))                                                                           [2, afterfib-n-1, 4, fib-done]
(test (op =) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n))
(goto (reg continue))                                                            1
(assign n (reg val))
(restore val)
(restore continue)                                                               2                [afterfib-n-1, 4, fib-done]
(assign val (op +) (reg val) (reg n))            afterfib-n-1                                     [4, fib-done]
(goto (reg continue))                                                            3
(restore n)
(assign n (op -) (reg n) (const 2))                                   4                           [fib-done]
(assign continue (label afterfib-n-2))                                2
(save val)                                       afterfib-n-2
(goto (label fib-loop))                                                                           [3, fib-done]
(test (op =) (reg n) (const 2))
(branch (label immediate-answer))
(assign val (reg n))
(goto (reg continue))                                                            2
(assign n (reg val))
(restore val)
(restore continue)                                                               3                [fib-done]
(assign val (op +) (reg val) (reg n))            fib-done
(goto (reg continue))                                                            5

