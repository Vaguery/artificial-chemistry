(ns artificial-chemistry.lexicase
   (:use [artificial-chemistry.core]))


(def posInfinity
  Double/POSITIVE_INFINITY)


(defn sort-by-error-item-with-NaN-worst
  [pile which]
  (sort-by 
    (fn [rm] (let [k (nth (first (:error-vector rm)) which)] (if (isNaN? k) posInfinity k)))
    (shuffle pile)))



(defn lexicase-cull-one
  [pile]
  (let [pop-size     (count pile)
        exemplar     (first pile)
        error-count  (count (first (:error-vector exemplar)))
        case-order   (shuffle (range error-count))]
    (loop [survivors pile
           criteria  case-order]
      (cond (empty? criteria) survivors
            (= 1 (count survivors)) survivors
            :else
              (let [criterion (first criteria)]
                (recur (butlast (sort-by-error-item-with-NaN-worst pile criterion))
                       (rest criteria)))))))



(defn lexicase-cull-many
  [pile keep]
  (loop [survivors pile]
    (if (<= (count survivors) keep)
      survivors
      (recur (lexicase-cull-one survivors)))))





(defn one-lexicase-step
  "Assumes machines are scored before arriving; shuffles and samples data for each evaluation"
  [pile scale-factor data mutation-rate mutation-stdev]
  (let [n (count pile)
        best (first pile)
        ro (count (:read-only best))
        scale (apply max (:read-only best))
        cxn (count (:connectors best))
        fxn-count (count (:program best))
        subset (take (/ (count data) 2) (shuffle data))
        ]

    (-> (shuffle pile)
        (into , (starting-pile n ro scale cxn fxn-count all-functions))
        (into , (generational-breed-many pile n mutation-rate mutation-stdev))
        (score-pile , scale-factor subset)
        (lexicase-cull-many , n)
        )))



(defn lexicase-search
  "Starts a long-running loop of generations, for the specified problem set, and starting with the specified pile of individuals. In each generation, the pile is increased by the indicated factor, mutation is applied, and data files are saved. Old data files are overwritten."
  [dataset dataname
    pop-size ro-count constant-scale connector-count function-count function-set 
    sampling-factor generations
    mse-weight failure-weight mutation-rate mutant-stdev]

  (let [start-pile (starting-pile pop-size
                                  ro-count 
                                  constant-scale 
                                  connector-count
                                  function-count
                                  function-set)
        start-pile (score-pile start-pile sampling-factor dataset)]

      (spit (str "lexicase-rms-" dataname ".csv") "")

      (loop [pile start-pile
             step 0]
        (if (or (> step generations) (< (:mse (first pile)) 0.0001))
          (do 
            (report-line (str "lexicase-rms-" dataname ".csv") step pile)
            (report-best (str "lexicase-rms-" dataname "-best.csv") step pile))
          (do
            (report-line (str "lexicase-rms-" dataname ".csv") step pile)
            (recur (one-lexicase-step 
                        pile sampling-factor dataset mutation-rate mutant-stdev)
                   (inc step))
                   )))))



