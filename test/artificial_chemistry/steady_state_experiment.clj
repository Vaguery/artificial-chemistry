(ns artificial-chemistry.steady-state-experiment
  (:use midje.sweet
       [artificial-chemistry.core]))


;;;; implementing a simple steady state experiment interactively


(defn random-x6-case
  []
  (let [x (+ (rand 100) -50)]
    [[x] (+ x 6)]
    ))


(def x6-data
  (repeatedly 100 random-x6-case))



(defn random-birthday-case
  []
  (let [x (+ (rand 100) -50)]
    [[x] (+ (* 9 x x) (* 11 x) 1964)]
    ))


(def birthday-data
  (repeatedly 100 random-birthday-case))


(def starting-pile
  (repeatedly 
    100 
    #(randomize-read-only
      (random-register-machine all-functions 11 30 100)
        10
        )))


(defn score-pile
  [dataset]
  (pmap #(record-errors % 5 (take 50 (shuffle dataset))) starting-pile))


(def scored-start-pile
  (score-pile birthday-data))

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
      #(+ (:mse %) (* 1e12 (:failures %)))
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
  [pile scale data]
  (let [baby (record-errors (steady-state-breed-one pile) scale data)
        mute (record-errors (mutate (rand-nth pile) 0.03 1.0) scale data)]
    (-> pile
        steady-state-cull-one
        (conj , mute)
        steady-state-cull-one
        (conj , baby)
    )))


(defn report-line
  [filename t pile]
  (do 
    (spit filename
          (str t ", "
            (clojure.string/join ", " (map :mse pile))
            "\n")
          :append true)
    (println
      (str t ", "
        (clojure.string/join ", " (take 5 (map :mse pile))) "..."
        ))))


(defn report-best
  [filename t pile]
  (do 
    (spit filename
          (str t ", "
            (pr-str (second pile))
            "\n")
          :append true)
          ))


(defn generational-breed-many
  "Takes a collection of RegisterMachines. Returns a collection of N new crossover products"
  [pile size stdev]
  (repeatedly size #(mutate (steady-state-breed-one pile) 0.03 1000)))


(fact
  (count (generational-breed-many starting-pile 20 1)) => 20)


(defn generational-cull-many
  [pile keep]
  (take keep 
    (sort-by
      #(+ (:mse %) (* 1e12 (:failures %)))
     (shuffle pile))))

(fact
  (count (generational-cull-many 
            (map #(record-errors % 5 x6-data) starting-pile) 3)) => 3)


(defn score-pile
  [pile scale-factor data]
  (pmap #(record-errors % scale-factor (take 50 (shuffle data))) pile))



(defn one-generational-step
  "Assumes machines are scored before arriving; shuffles and samples data for each evaluation"
  [pile scale-factor data stdev]
  (let [n (count pile)]
  (into []
    (-> pile
      (into , (generational-breed-many pile n stdev))
      (score-pile , scale-factor data)
      (generational-cull-many , n)
      ))))


(defn multiple-score-samples
  "Takes a RegisterMachine, a scale factor, a dataset, and a number of replications to sample. Returns the `:mse` measured for each replication, on each of the training cases, for just that machine."
  [rm scale-factor data replicates]
  (take replicates 
    (iterate #(record-errors % scale-factor data) rm)))


; (fact
;   (map :error-vector
;     (multiple-score-samples (first starting-pile) 5 (take 100 x6-data) 5)) => 9)

(do
  (spit "generational-rms-bday.csv" "")
  (loop [pile scored-start-pile
         step 0]
    (if (or (> step 5000) (< (:mse (first pile)) 0.0001))
      (do 
        (report-line "generational-rms-bday.csv" step pile)
        (report-best "generational-rms-bday-best.csv" step pile))
      (do
        (report-line "generational-rms-bday.csv" step pile)
        (recur (one-generational-step pile 5 birthday-data 1000)
               (inc step))
               ))))


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

