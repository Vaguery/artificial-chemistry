(ns artificial-chemistry.tangle
  (:use midje.sweet
        [artificial-chemistry.core]
        clojure.java.io)
  (:require [tangle.core :as tangle]))



(def test-program
  (random-program all-functions 11 30 100))


(defn step-labels
  [prog]
  (into [] 
    (map-indexed 
    (fn [idx step] (str (inc idx) "-" (:string step))) 
    prog)))


(fact 
  (count (step-labels test-program)) => (count test-program)
  )


(defn register-nodes
  [prog]
  (into [] 
    (map (fn [n] n) 
      (sort 
        (distinct 
          (flatten (conj (map :args prog) (map :target prog))))))))


(defn step-nodes
  [prog]
  (map
    (fn [step] step)   ;;; somehow need to assign attributes here
    (step-labels prog)
    ))

; (println (step-nodes test-program))

(def nodes 
  (concat (step-nodes test-program) (register-nodes test-program)))

; (println nodes)


(defn all-arg-edges
  [prog]
  (reduce
    concat
    (map
      (fn [step label] 
        (map #(vector  % label) (:args step)))
      prog
      (step-labels prog))))


(defn all-target-edges
  [prog]
  (map
    (fn [step label]
      (vector label (:target step)))
    prog
    (step-labels prog)))




(def edges (concat (all-arg-edges test-program) (all-target-edges test-program)))


(def dot (tangle/graph->dot 
            nodes 
            edges 
            {:directed? true
            }))


; (println dot)

; (copy (tangle/dot->svg dot) (file "test.svg"))
