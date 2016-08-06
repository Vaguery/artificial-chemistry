(ns artificial-chemistry.runners
  (:use midje.sweet)
  (:use artificial-chemistry.core)
  (:use artificial-chemistry.data))


(fact :generational
  "run a generational search"
  (generational-search 
    birthday-data "bday" 
    100 11 1e6 30 50 all-functions 
    5 500 
    1 1e12 0.02 1000) => 99)

