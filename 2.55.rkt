#lang sicp
(newline)


(display "(car ''abracadabra) => ")
(car ''abracadabra)

(display "

This prints \"quote\" because the evaluator reads the expression as
    (car (quote (quote abracadabra)))

which is the same as
    (car '(quote abracadabra))
or
    (car (list \'quote \'abracadabra))

")
