(ns artificial-chemistry.steady-state-experiment
  (:use midje.sweet
       [artificial-chemistry.core]
       [artificial-chemistry.data]))




 ; [dataset dataname
 ;    pop-size ro-count constant-scale connector-count function-count function-set 
 ;    sampling-factor generations
 ;    mse-weight failure-weight mutant-stdev]












; (do
;   (spit "steady-state-rms.csv" "")
;   (loop [pile scored-start-pile
;          step 0]
;     (if (or (> step 50000) (< (:mse (first pile)) 0.001))
;       (do 
;         (report-line "steady-state-rms.csv" step pile)
;         (report-best "steady-state-rms-best.csv" step pile))
;       (do
;         (report-line "steady-state-rms.csv" step pile)
;         (recur (one-steady-state-step pile 5 x6-data)
;                (inc step))
;                ))))

