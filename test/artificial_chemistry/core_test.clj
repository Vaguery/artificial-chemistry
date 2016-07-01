(ns artificial-chemistry.core-test
  (:use midje.sweet)
  (:use [artificial-chemistry.core]))


;; Based on http://www.cs.mun.ca/~banzhaf/chapters.html#Genetic%20Programming%20of%20an%20Algorithmic%20Chemistry


(def constant-registers 
  (map #(keyword (str "k" (char (+ 65 %)))) (range 11)))


(fact 
  constant-registers => [:kA :kB :kC :kD :kE :kF :kG :kH :kI :kJ :kK])


(def connection-registers 
  (map #(keyword (str "c" %)) (range 35)))


(fact 
  connection-registers => [:c0 :c1 :c2 :c3 :c4 :c5 :c6 :c7 :c8 :c9 :c10 :c11 :c12 :c13 :c14 :c15 :c16 :c17 :c18 :c19 :c20 :c21 :c22 :c23 :c24 :c25 :c26 :c27 :c28 :c29 :c30 :c31 :c32 :c33 :c34])