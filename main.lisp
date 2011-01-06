;; sc2 strategy analyzer

;; rule based

(defun population-of (unit)
  (cond ((eq unit 'queen) 2)
        ((eq unit 'zergline) 1)
        ((eq unit 'roach) 2)
        ((eq unit 'drone) 1)
        ((eq unit 'overlord) 0)
        (t -1)))

(defun define-rule (rule-name rule)
  "Define a rule. rule: list"
  (loop 
     for round in 
       (let ((current-population 6))
         (loop 
            for sequence in rule
            for n from 0
            collect
            (cond ((= (length sequence) 1)
                   (let ((unit (car sequence)))
                     (setf current-population
                           (+ current-population (population-of unit)))
                     unit))
                  (t
                   (let* ((drone-number (car sequence))
                          (build (cdr sequence))
                          (population-delta (loop 
                                               for unit in build
                                               sum 
                                               (population-of unit))))
                     (let ((drone-to-produce (- drone-number current-population)))
                       (setf current-population 
                             (+ drone-number population-delta))
                       (let ((sequence (loop
                                          for i from 1 to drone-to-produce
                                          collect
                                          'drone)))
                         (append sequence build))))))))
     nconc round))
                       
;;; start to define a bunch of zerg strategy

(define-rule '10-pool '((10 pool)
                        (10 overlord)
                        (12 zergling zergline zergline)))

(define-rule '10-pool-speedling '((10 pool)
                                  (10 extractor)
                                  (10 overlord zergline zergline zergline)
                                  (drone)))

(define-rule '13-pool '((10 overlord)
                        (13 pool)))


(define-rule '14-hatch-14-pool '((9 overlord)
                                 (14 hatch)
                                 (14 pool)
                                 (16 extractor)
                                 (16 overlord)
                                 (17 queen queen)))

(define-rule '14-gas-14-pool '((14 extractor)
                               (14 pool)))


(define-rule '14-pool-baneling-basic '((9 overlord)
                                       (14 pool extractor)
                                       (14 overlord zergline zergline zergline queen)
                                       (20 zergline zergline baneling-nest)
                                       (22 overlord)))

(define-rule '14-pool-baneling-safe '((9 overlord)
                                      (14 pool extractor)
                                      (14 overlord zergline zergline zergline queen)
                                      (20 zergline zergline zergline zergline zergline zergline baneling-nest lair)
                                      (26 extractor overlord)))

(define-rule '14-pool-15-hatch '((9 overlord)
                                 (14 pool)
                                 (15 hatch)
                                 (16 queen extractor)
                                 (18 overlord)))

(define-rule '15-pool-fast-expand '((9 overlord)
                                    (15 pool)
                                    (16 hatch)))


(define-rule '1-base-roach '((9 overlord)
                             (13 pool)
                             (15 extractor)
                             (16 roach-warren queen overlord overlord roach roach roach roach roach roach roach roach)
                             (overlord)))

(define-rule '2-hatch-speedling-muta '((9 overlord)
                                       (14 extractor)
                                       (15 pool)
                                       (16 overlord queen lair hatch spire)))


(define-rule 'ling-speed-lair '((9 overlord)
                                (14 extractor pool)
                                (16 overlord queen lair hatch)))

(define-rule 'ling-speed-lair-2-base '((9 overlord)
                                       (14 extractor)
                                       (14 pool)
                                       (16 overlord queen hatch lair)))

(define-rule 'roach-build '((10 overlord)
                            (15 pool)
                            (15 extractor)
                            (17 roach-warren queen overlord)))


                                       
                                
                                
                                       
                             
                             
                                 
                                      
                                      



                               