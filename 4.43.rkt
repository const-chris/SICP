#lang sicp
(#%require (file "./4.39.rkt"))

(define (fathers)
  (let ((mary-ann 'moore)
        (melissa  'hood)
        (gabrielle (amb 'downing 'hall 'hood 'parker)))
    (require (not (eq? gabrielle 'hood)))
    (require (not (eq? gabrielle 'parker)))
    (let ((rosalind (amb 'downing 'hall 'hood 'parker)))
      (require (not (eq? rosalind 'hall)))
      (let ((lorna (amb 'downing 'hall 'hood 'parker)))
        (let ((yachts `((moore ,lorna) (downing ,melissa) (hall ,rosalind) (hood ,gabrielle))))
          (require (eq? (cadr (assoc gabrielle yachts)) 'parker))
          (require (distinct? (list gabrielle lorna mary-ann melissa rosalind)))
          (list 'lorna lorna))))))

(fathers)

#|
If we don't know Mary-Ann's last name, there are two solutions:
|#

(input-definition '(define (fathers)
                    (let ((melissa  'hood)
                          (gabrielle (amb 'downing 'hall 'hood 'parker 'moore)))
                      (require (not (eq? gabrielle 'hood)))
                      (require (not (eq? gabrielle 'parker)))
                      (let ((rosalind (amb 'downing 'hall 'hood 'parker 'moore)))
                        (require (not (eq? rosalind 'hall)))
                        (let ((lorna (amb 'downing 'hall 'hood 'parker 'moore)))
                          (require (not (eq? lorna 'moore)))
                          (let ((mary-ann (amb 'downing 'hall 'hood 'parker 'moore)))
                            (let ((yachts (list (list 'moore lorna)
                                                (list 'downing melissa)
                                                (list 'hall rosalind)
                                                (list 'hood gabrielle))))
                              (require (eq? (cadr (assoc gabrielle yachts)) 'parker))
                              (require (distinct? (list gabrielle lorna mary-ann melissa rosalind)))
                              (list  'lorna lorna))))))))

(driver-loop)
