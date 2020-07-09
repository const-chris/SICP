#lang sicp
(#%require (file "./natural-language-parser.rkt"))
(driver-loop)

#|
For the sentence "The professor lectures to the student in the class with the cat."
The five parses are:

1.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb-phrase (verb lectures)
                   (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

The professor is the one in class, and the professor is the one with the cat.


2.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
    (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class))
                                        (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

Differs from the first parse in that now the cat belongs to the class.


3.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb-phrase
      (verb lectures)
      (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student))
                                          (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

The cat is back with the professor, but now the student is the one in class.


4.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase (prep to) (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student))
                                                     (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
                                        (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

The student is in class, and the student has the cat.


5.
(sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
    (verb lectures)
    (prep-phrase (prep to) (noun-phrase (simple-noun-phrase (article the) (noun student))
                                        (prep-phrase (prep in) (noun-phrase (simple-noun-phrase (article the) (noun class))
                                                                            (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))))

The student is still in class, but now the cat belongs to the class.
|#
