(ns artificial-chemistry.data)


;; y=x+6 problem


(defn random-x6-case
  []
  (let [x (+ (rand 100) -50)]
    [[x] (+ x 6)]
    ))


(def x6-training-data
  (repeatedly 100 random-x6-case))


;; birthday problem


(defn random-birthday-case
  []
  (let [x (+ (rand 100) -50)]
    [[x] (+ (* 9 x x) (* 11 x) 1964)]
    ))


(def birthday-data
  (repeatedly 100 random-birthday-case))