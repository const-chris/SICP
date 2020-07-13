#lang sicp
(#%require (file "./microshaft-db.rkt"))

(add-assertion! '(meeting accounting (Monday 9am)))
(add-assertion! '(meeting administration (Monday 10am)))
(add-assertion! '(meeting computer (Wednesday 3pm)))
(add-assertion! '(meeting administration (Friday 1pm)))
(add-assertion! '(meeting whole-company (Wednesday 4pm)))

#| a.
to query for all meetings that occur on Friday:
  (meeting ?division (Friday . ?time))

  (meeting administration (Friday 1pm))
|#


;; b.
(run-query
  '(assert! (rule (meeting-time ?person ?day-and-time)
                  (or (meeting whole-company ?day-and-time)
                      (and (meeting ?division ?day-and-time)
                           (job ?person (?division . ?subdivision)))))))


#| c.
to query for all of Alyssa's meetings on Wednesday:
  (meeting-time (Hacker Alyssa P) (Wednesday . ?time))

  (meeting-time (Hacker Alyssa P) (Wednesday 4pm))
  (meeting-time (Hacker Alyssa P) (Wednesday 3pm))
|#

(query-driver-loop)
