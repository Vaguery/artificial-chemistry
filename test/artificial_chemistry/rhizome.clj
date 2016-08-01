(ns artificial-chemistry.rhizome
  (:use midje.sweet
        [artificial-chemistry.core])
  (:require [rhizome.viz :as rhizome]))


(def g
    {:a [:b :c]
     :b [:c]
     :c [:a]})



(def test-program
  (random-program all-functions 11 30 100))


(defn step-labels
  [prog]
  (map-indexed 
    (fn [idx step] (str (inc idx) "-" (:string step))) prog)
  )

(fact 
  (count (step-labels test-program)) => (count test-program))


(defn store-arg-edge
  [graph arg step]
  (let [edges (get graph arg [])]
    (assoc graph arg (conj edges step))
    ))

(fact
  (store-arg-edge {} 12 "FOO") => {12 ["FOO"]}
  (store-arg-edge {12 ["FOO"]} 12 "BAR") => {12 ["FOO" "BAR"]}
  (store-arg-edge {12 ["FOO"]} 2 "BAR") => {2 ["BAR"], 12 ["FOO"]})


(defn store-out-edge
  [graph step target]
  (let [edges (get graph step [])]
    (assoc graph step (conj edges target))
    ))


(fact
  (store-out-edge {} "FOO" 1) => {"FOO" [1]}
  (store-out-edge {"FOO" [1]} "FOO" 9) => {"FOO" [1 9]}
  (store-out-edge {"FOO" [1 9]} "BAR" 9) => {"BAR" [9], "FOO" [1 9]})


(defn store-step-edges
  [graph args step target]
  (-> (reduce #(store-arg-edge %1 %2 step) graph args)
      (store-out-edge , step target)
  ))


(fact
  (store-step-edges {} [1 2 3] "1-FOO" 9) =>
    {1 ["1-FOO"], 2 ["1-FOO"], 3 ["1-FOO"], "1-FOO" [9]}
  (store-step-edges {1 ["1-FOO"], 2 ["1-FOO"], 3 ["1-FOO"], "1-FOO" [9]}
                    [1 2] "2-BAR" 2) => 
    {1 ["1-FOO" "2-BAR"], 2 ["1-FOO" "2-BAR"], 3 ["1-FOO"], "1-FOO" [9], "2-BAR" [2]}
    )


(defn store-program
  [prog]
  (let [in     (map :args prog)
        out    (map :target prog)
        labels (step-labels prog)
        nodes  (partition 3 (interleave in labels out))]
    (reduce
      (fn [g [a b c]] (store-step-edges g a b c))
      {}
      nodes)))

(def g (store-program test-program))

(rhizome/view-graph (keys g) g
    :node->descriptor (fn [n] {:label n})
    :options {:mode :spring}
    )

