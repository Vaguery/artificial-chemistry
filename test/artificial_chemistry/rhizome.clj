(ns artificial-chemistry.rhizome
  (:use midje.sweet
        [artificial-chemistry.core])
  (:require [rhizome.viz :as rhizome]))


(def g
    {:a [:b :c]
     :b [:c]
     :c [:a]})

; (rhizome/view-graph (keys g) g
;     :node->descriptor (fn [n] {:label n}))


(def test-program
  (random-program all-functions 2 2 3))


; (defn record-args-edges
;   [graph idx step]
;   (let [args  (:args step)
;         text  (:string step)]
;     (reduce
;       (fn [g a]
;         (let [edges (get g a [])]
;           (assoc g a (conj edges text))))
;       graph
;       args)
;     ))


; (fact
;   (reduce 
;     (fn [g s] (record-args-edges g s))
;     {}
;     test-program) => 99
;   )


; (defn dotfile-for
;   [prog]
;   (reduce
;     #()
;     {}
;     prog))

; (fact "a program can be converted into a dot string"
;   (dotfile-for test-program) => 3
;   )