(ns artificial-chemistry.linear-test
  (:use midje.sweet)
  (:use [artificial-chemistry.core])
  (:use [artificial-chemistry.linear])
  (:require [roul.random :as rr]
            [artificial-chemistry.data :as data]))

(fact "I can run a program through with invoke-ordered-program"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6]
              [(->ProgramStep :foo * [0 1] 2)
               (->ProgramStep :foo * [1 2] 1)
               (->ProgramStep :foo + [4 5] 0)
               ])]

  (:connectors (invoke-ordered-program rm)) => [128 56 72]
 ))


(fact "I can read the output of an invoked program"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6]
              [(->ProgramStep :foo * [0 1] 2)
               (->ProgramStep :foo * [1 2] 1)
               (->ProgramStep :foo + [4 5] 0)
             ])]
  (output (invoke-ordered-program rm)) => 72
  ))



(fact "I can get error-vector-ordered of an invoked program"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6]
              [(->ProgramStep :foo * [0 1] 2)
               (->ProgramStep :foo * [1 2] 1)
               (->ProgramStep :foo + [4 5] 0)
             ])]
  (error-vector-ordered rm [[[1] 2] [[2] 3]] ) => [70 69]
  ))




(fact "crossover-program-ordered"
  (let [mom (->RegisterMachine [1 2] [1 2] [:a :b :c :d :e])
        dad (->RegisterMachine [1 2] [1 2] [:A :B :C :D :E])]

  (crossover-program-ordered mom dad 200) => [:a :b :C :D :E]
    (provided (rand-int 5) => 2)
  ))



(fact "crossover-ordered"
  (let [mom (->RegisterMachine [1 2] [1 2] [:a :b :c :d :e])
        dad (->RegisterMachine [3 4] [1 2] [:A :B :C :D :E])]

  (crossover-ordered mom dad 200) => (->RegisterMachine [:X :Y] [1 2] [:a :b :C :D :E])
    (provided (rand-int 5) => 2
              (rand-nth anything) =streams=> [:X :Y])
  ))



(fact :linearGP
  "run a linearGP search on y=9x^2+11x+1964"
  (linearGP-search 
    (take 10 data/birthday-data) "bday" 
    100 11 10.0 30 50 all-functions 
    3 200 5000 
    1 1e12 0.03 1.0) => 99)


; (fact :linearGP
;   "run a linearGP search on y=x+6"
;   (linearGP-search 
;     (take 10 data/x6-training-data) "x6" 
;     100 11 100.0 30 100 all-functions 
;     5 200 500 
;     1 1e12 0.05 1.0) => 99)


; (fact :linearGP
;   "run a linearGP search on y=sin(x)"
;   (linearGP-search 
;     data/sine-data "sine" 
;     100 11 10 30 50 all-functions 
;     5 200 500 
;     1 1e12 0.05 1) => 99)