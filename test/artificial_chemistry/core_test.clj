(ns artificial-chemistry.core-test
  (:use midje.sweet)
  (:use [artificial-chemistry.core]))


(fact "I can make a new RegisterMachine"
  (:read-only (->RegisterMachine [1 2 3] [4 5 6] [:foo])) => [1 2 3]
  (:connectors (->RegisterMachine [1 2 3] [4 5 6] [:foo])) => [4 5 6]
  (:program (->RegisterMachine [1 2 3] [4 5 6] [:foo])) => [:foo]
  )


(fact "I can read `:read-only` and `:connectors` registers"
  (let [testing (->RegisterMachine [1 2 3] [4 5 6] [:foo])]
    (value-of testing 0) => 1
    (value-of testing 3) => 4
    (value-of testing 5) => 6
  ))


(fact "I can write to `:connectors` registers"
  (let [testing (->RegisterMachine [1 2 3] [4 5 6] [:foo])]
    (:connectors (write-value testing 1 99.99)) => [4 99.99 6]
  ))


(fact "I can make a new ProgramStep"
  (:function (->ProgramStep '+' [9 12] 3)) => '+'
  (:args     (->ProgramStep '+' [9 12] 3)) => [9 12]
  (:target   (->ProgramStep '+' [9 12] 3)) => 3
  )


(fact "I can invoke a ProgramStep 'on' a RegisterMachine"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6] [:foo])]

    (invoke (->ProgramStep +' [5 1] 0) rm) =>
      (->RegisterMachine [9 8 7] [14 5 6] [:foo])

    (invoke (->ProgramStep -' [2 1] 0) rm) =>
      (->RegisterMachine [9 8 7] [-1 5 6] [:foo])

    (invoke (->ProgramStep *' [0 3] 1) rm) =>
      (->RegisterMachine [9 8 7] [4 36 6] [:foo])

    (invoke (->ProgramStep pdiv [4 3] 0) rm) =>
      (->RegisterMachine [9 8 7] [5/4 5 6] [:foo])
  ))


(fact "protected division returns 1.0 instead of blowing up"
  (let [rm (->RegisterMachine [0 0 0] [0 0 0] [:foo])]
    (invoke (->ProgramStep pdiv [4 3] 0) rm) =>
      (->RegisterMachine [0 0 0] [1.0 0 0] [:foo])
  ))


(fact "exponentiation doesn't blow up"
  (let [rm (->RegisterMachine [-2 1/4] [0] [:foo])]
    (Double/isNaN (first (:connectors 
        (invoke (->ProgramStep pow [0 1] 0) rm)))) =>
      true
))



(fact "rm-not treats a numeric argument as if it were a boolean"
  (rm-not 9) => 0.0
  (rm-not 0.0) => 1.0
  (rm-not 1.0) => 0.0
  (rm-not -9/13) => 0.0
  )



(fact "rm-and treats its arguments as if they were booleans"
  (rm-and 9 8) => 1.0
  (rm-and 9 0) => 0.0
  (rm-and 0 0) => 0.0
  (rm-and 0 1) => 0.0
  (rm-and -3 -2) => 1.0
  )



(fact "rm-or treats its arguments as if they were booleans"
  (rm-or 9 8) => 1.0
  (rm-or 9 0) => 1.0
  (rm-or 0 0) => 0.0
  (rm-or 0 1) => 1.0
  (rm-or -3 -2) => 1.0
  )



(fact "random-program-step"
  (:args (random-program-step all-functions 10 12)) => [17 17]
    (provided (rand-nth all-functions) => (first all-functions)
              (rand-int 22) => 17
              (rand-int 12) => 7)
  (:target (random-program-step all-functions 10 12)) => 7
    (provided (rand-int 22) => 17
              (rand-int 12) => 7)
  )


(fact "random-program-step with arity 1"
  (random-program-step all-functions 10 12) =>
    (->ProgramStep rm-not [17] 7)
  (provided (rand-nth all-functions) => [rm-not 1],
            (rand-int 22) => 17
            (rand-int 12) => 7))


(fact "I can create and invoke a random-program-step"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6] [:foo])]
    (invoke (random-program-step all-functions 3 3) rm) =>
      (->RegisterMachine [9 8 7] [49 5 6] [:foo])
        (provided (rand-nth all-functions) => (second all-functions)
                  (rand-int 6) => 2
                  (rand-int 3) => 0)

  ))


(fact "random-program returns a collection of ProgramStep items"
  (count (random-program all-functions 10 10 100)) => 100
  (random-program all-functions 10 10 100) => (repeat 100 999)
    (provided (random-program-step all-functions 10 10) => 999)
  )




(fact "I can examine the state after a specified number of steps with invoke-many-steps"
  (let [rm (->RegisterMachine
              (into [] (take 5 (repeatedly #(rand 100.0))))
              (into [] (take 5 (repeatedly #(rand-int 100))))
              (random-program all-functions 5 5 20))]
    (class (invoke-many-steps rm 0)) => (class rm)
  ))


(fact "I can trace steps with rm-trace"
  (let [rm (->RegisterMachine
              (into [] (take 11 (repeatedly #(rand 100.0))))
              (into [] (repeat 30 0.0))
              (random-program all-functions 11 30 100))]
    
    (count (rm-trace rm 50)) => 50
    (distinct (map class (rm-trace rm 50))) => [clojure.lang.PersistentVector]
    (distinct (map count (rm-trace rm 50))) => [30]

  ))


(fact "output returns the value of the last element of `:connectors`"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6] [:foo])]
    (output rm) => 6
  ))


(fact "set-inputs"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6] [:foo])]
    (set-inputs rm [99 99]) => (->RegisterMachine [9 8 7] [99 99 6] [:foo])
    (class (:connectors (set-inputs rm [99 99]))) => clojure.lang.PersistentVector
))



(fact "output-given-inputs steps a bunch and returns a number"
  (let [rm (->RegisterMachine [1] [1] [(->ProgramStep +' [0 1] 0)])]
    (output-given-inputs rm 5 [99]) => 104
    (output-given-inputs rm 1 [99]) => 100
    (output-given-inputs rm 0 [99]) => 99
  ))


(fact "random-sine-case returns a vector"
  (let [rsc (random-sine-case)]
    (and  (>= (ffirst rsc) (- Math/PI)) (<= (ffirst rsc) Math/PI)) => true
    (second rsc) => (Math/sin (ffirst rsc))
  ))


(fact "output-vector"
  (let [rm (->RegisterMachine [1] [1] [(->ProgramStep +' [0 1] 0)])]
    (output-given-inputs rm 5 [99]) => 104
    (output-vector rm 5 (list [[99] 888] [[17] 888]) ) => [104 22]
    ))


(fact "error-vector"
  (let [rm (->RegisterMachine [1] [1] [(->ProgramStep +' [0 1] 0)])]
    (error-vector rm 5 (list [[99] 100] [[17] 100]) ) => [4 78]
    ))


(fact "I can apply error-vector to sine-data"
  (let [rm (->RegisterMachine [1] [1] [(->ProgramStep +' [0 1] 0)])]
    (count (error-vector rm 100 sine-data)) => (count sine-data)
    ))


(fact "mean-squared-error does what the sign says"
  (mean-squared-error [0 0 0 0 0 0]) => 0
  (mean-squared-error [0 0 0 0 0 1]) => 1
  (mean-squared-error [0 0 0 0 0 2]) => 4
  (mean-squared-error [2 2 2 2 2 2]) => 24
  )


(fact "errors-and-failures returns a hash with both scores"
  (let [rm (->RegisterMachine [1] [1] [(->ProgramStep +' [0 1] 0)])]
    (keys (errors-and-failures rm 500 sine-data)) => [:mse :failures :error-vector]
  ))


(fact "record-errors modifies a RegisterMachine"
  (let [rm (->RegisterMachine [1] [1] [(->ProgramStep +' [0 1] 0)])]
    (keys (record-errors rm 500 sine-data)) =>
      [:read-only :connectors :program :error-vector :mse :failures]
))


(fact "crossover-registers"
  (let [mom (->RegisterMachine [1 1 1 1] [0 0 0] [:a :b :c :d])
        dad (->RegisterMachine [2 2 2 2] [0 0 0] [:z :y :x :w])]
    (crossover-registers mom dad) => [1 2 1 2]
    (provided (rand) =streams=> [0 1 0 1])
    ))


(fact "crossover-program"
  (let [mom (->RegisterMachine [1 1 1 1] [0 0 0] [:a :b :c :d])
        dad (->RegisterMachine [2 2 2 2] [0 0 0] [:z :y :x :w])]
    (crossover-program mom dad) => [:MOM :MOM :MOM :MOM :DAD :DAD :DAD :DAD]
    (provided (rand-int 4) => 4
              (rand-nth [:a :b :c :d]) => :MOM
              (rand-nth [:z :y :x :w]) => :DAD
              )))



(fact "crossover"
  (let [mom (->RegisterMachine [1 1 1 1] [0 0 0] [:a :b :c :d])
        dad (->RegisterMachine [2 2 2 2] [0 0 0] [:z :y :x :w])]
    (crossover mom dad) => (->RegisterMachine
                              [1 2 1 2]
                              [0 0 0]
                              [:MOM :MOM :MOM :MOM :DAD :DAD :DAD :DAD ])
    
    (provided (rand-int 4) => 4
              (rand-nth [:a :b :c :d]) => :MOM
              (rand-nth [:z :y :x :w]) => :DAD
              (rand) =streams=> [0.1 0.8 0.1 0.8])))


(fact "mutate-registers"
  (let [mom (->RegisterMachine [1 1 1 1] [0 0 0] [:a :b :c :d])]
    (:read-only (mutate-registers mom 0)) => (:read-only mom)
    (:read-only (mutate-registers mom 1)) =not=> (:read-only mom)
  ))


(fact "mutate-program"
  (let [mom (->RegisterMachine [1 1 1 1] [0 0 0] [:a :b :c :d])]
    (:program (mutate-program mom 0)) => (:program mom)
    (:program (mutate-program mom 1)) =not=> (:program mom)
    (mutate-program mom 0) => mom
  ))


;;;; implementing steady state experiment



(defn random-x6-case
  []
  (let [x (+ (rand 100) -50)]
    [[x] (+ x 6)]
    ))


(def x6-data
  (repeatedly 100 random-x6-case))



(defn apply-rubrics
  [rm]
  (record-errors rm 500 (take 10 (shuffle sine-data))))


(defn random-sine-machine
  []
  (->RegisterMachine
    (into [] (repeatedly 11 #(- (rand 10.0) 5.0)))
    (into [] (repeat 30 0.0))
    (random-program all-functions 11 30 100)))


(def starting-pile
  (repeatedly 100 #(random-sine-machine)))


(defn score-pile
  [pile]
  (pmap apply-rubrics pile))


(defn steady-state-breed
  [pile]
  (let [mom  (rand-nth pile)
        dad  (rand-nth pile)
        baby (mutate (crossover mom dad) 0.1)
        scored-baby (apply-rubrics baby)]
    (conj pile scored-baby)))


(defn steady-state-immigrate
  [pile]
  (conj pile (apply-rubrics (random-sine-machine))))



(defn steady-state-cull
  [pile kill]
  (drop-last kill (sort-by #(+ (:mse %) (* 10 (:failures %))) (shuffle pile))))


(defn print-mse-scores
  [pile t]
  (let [line (str "\n" t ", " (clojure.string/join ", " (map :mse pile)))]
    (spit "steady-state-rms.csv" line :append true)
    (println t " " (:mse (first pile)))
    pile))


(loop [pile (score-pile starting-pile)
          t 0]
  (let [primo (first pile)]
    (if (or (> t 10000) (and (zero? (:mse primo)) (zero? (:failures primo))))
      (println (first pile))
      (recur (-> pile
                 steady-state-immigrate
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 steady-state-breed
                 score-pile
                 (steady-state-cull , 11)
                 (print-mse-scores, t))
              (inc t)
              ))))
