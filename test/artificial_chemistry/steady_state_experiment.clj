(ns artificial-chemistry.steady-state-experiment
  (:use midje.sweet)
  (:use [artificial-chemistry.core]))


;;;; implementing a simple steady state experiment interactively


(defn random-x6-case
  []
  (let [x (+ (rand 100) -50)]
    [[x] (+ x 6)]
    ))


(def x6-data
  (repeatedly 100 random-x6-case))



(def starting-pile
  (repeatedly 
    20 
    #(randomize-read-only
      (random-register-machine all-functions 11 30 100)
        100
        )))


(def scored-start-pile
  (map #(record-errors % 500 x6-data) starting-pile))

; (println 
;   (map :mse scored-start-pile))


(defn steady-state-breed-one
  "Takes a collection of RegisterMachines. Returns one new crossover product"
  [pile]
  (let [mom  (rand-nth pile)
        dad  (rand-nth pile)]
    (crossover mom dad)
    ))


; (println
;   (:mse (record-errors (steady-state-breed-one starting-pile) 20 x6-data)))


(defn steady-state-cull-one
  [pile]
  (butlast 
    (sort-by
      #(+ (:mse %) (* 100000 (:failures %)))
     (shuffle pile))))


(fact "culling removes a highest-scoring from a pile"
  (count (map :mse scored-start-pile)) => 
    (inc (count (map :mse (steady-state-cull-one scored-start-pile))))
  (map :mse scored-start-pile) =>
    (contains (map :mse (steady-state-cull-one scored-start-pile))
      :in-any-order :gaps-ok )
    )


(defn one-steady-state-step
  "Assumes the machines are scored before arriving"
  [pile steps data]
  (let [baby (record-errors (steady-state-breed-one pile) steps data)]
    (conj (steady-state-cull-one pile) baby)
    ))


(defn report-line
  [t pile]
  (do 
    (spit "steady-state-rms.csv"
          (str t ", "
            (clojure.string/join ", " (map :mse pile))
            "\n")
          :append true)
    (println
      (str t ", "
        (clojure.string/join ", " (take 5 (map :mse pile))) "..."
        ))))


(do
  (spit "steady-state-rms.csv" "")
  (loop [pile scored-start-pile
         step 0]
    (if (> step 10000)
      (report-line step pile)
      (do
        (report-line step pile)
        (recur (one-steady-state-step pile 500 x6-data)
               (inc step))
               ))))

