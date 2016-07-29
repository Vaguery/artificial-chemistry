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


(fact "I can invoke a random program step of a RegisterMachine, and it will make a change"
  (let [rm (->RegisterMachine
              (into [] (take 5 (repeatedly #(rand 100.0))))
              (into [] (take 5 (repeatedly #(rand-int 100))))
              (random-program all-functions 5 5 20))]
    (count (clojure.set/intersection 
      (into #{} (:connectors (invoke-any-step rm)))
      (into #{} (:connectors rm)))) => 4
  ))



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
