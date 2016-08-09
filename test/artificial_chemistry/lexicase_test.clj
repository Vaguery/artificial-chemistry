(ns artificial-chemistry.lexicase-test
  (:use midje.sweet)
  (:use [artificial-chemistry.core]
        [artificial-chemistry.lexicase]
        [artificial-chemistry.data]))


(def lexicase-fixture
  [ (assoc (random-register-machine all-functions 3 3 1) :error-vector  '[[0 1 2 3 4]])
    (assoc (random-register-machine all-functions 3 3 1) :error-vector  '[[9 9 9 9 9]])
  ])


(fact "lexicase-cull-one"
  (lexicase-cull-one lexicase-fixture) => (list (first lexicase-fixture))
  )


(def theNan
  (- Double/POSITIVE_INFINITY Double/POSITIVE_INFINITY))



(def NaN-fixture
  [
    (assoc (random-register-machine all-functions 3 3 1)
      :error-vector [(list theNan)])
    (assoc (random-register-machine all-functions 3 3 1)
      :error-vector  [[0]])
  ])


(fact "lexicase-cull-one with NaN value(s)"
  (lexicase-cull-one NaN-fixture) => (list (last NaN-fixture))
  )





; (fact :lexicase
;   "run a lexicase search"
;   (lexicase-search 
;     birthday-data "bday" 
;     100 11 1e6 30 50 all-functions 
;     5 500 
;     1 1e12 0.05 1e3) => 99)


; (fact :lexicase
;   "run a lexicase search on y=11x "
;   (lexicase-search 
;     x-times-11-data "11x" 
;     100 11 1e6 30 50 all-functions 
;     5 500 
;     1 1e12 0.02 1000) => 99)


