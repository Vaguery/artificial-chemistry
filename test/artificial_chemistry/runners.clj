(ns artificial-chemistry.runners
  (:use midje.sweet)
  (:use artificial-chemistry.core)
  (:use artificial-chemistry.data))


(fact :generational
  "run a generational search on y=9x^2+11x+1964"
  (generational-search 
    birthday-data "bday" 
    100 11 1e6 30 50 all-functions 
    5 500 
    1 1e12 0.02 1000) => 99)

; (fact :generational
;   "run a generational search on y=1964 (constant)"
;   (generational-search 
;     year-data "year" 
;     100 11 1e6 30 50 all-functions 
;     5 500 
;     1 1e12 0.02 1000) => 99)

; (fact :generational
;   "run a generational search on y=11x "
;   (generational-search 
;     x-times-11-data "11x" 
;     100 11 1e6 30 50 all-functions 
;     5 500 
;     1 1e12 0.02 1000) => 99)


; (fact :generational
;   "run a generational search on y=x^2"
;   (generational-search 
;     x-squared-data "x^2" 
;     100 11 100 30 50 all-functions 
;     5 500 
;     1 1e12 0.05 10) => 99)

; (fact :generational
;   "run a generational search on y=x+6"
;   (generational-search 
;     (take 10 x6-training-data) "x6.10" 
;     100 11 100 30 20 all-functions 
;     5 500 
;     1 1e12 0.05 10) => 99)

