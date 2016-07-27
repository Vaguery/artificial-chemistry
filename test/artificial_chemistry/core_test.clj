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
  (:function (->ProgramStep '+ [9 12] 3)) => '+
  (:args     (->ProgramStep '+ [9 12] 3)) => [9 12]
  (:target   (->ProgramStep '+ [9 12] 3)) => 3
  )


(fact "I can invoke a ProgramStep 'on' a RegisterMachine"
  (let [rm (->RegisterMachine [9 8 7] [4 5 6] [:foo])]
    (invoke (->ProgramStep + [5 1] 0) rm) =>
      (->RegisterMachine [9 8 7] [14 5 6] [:foo])

    (invoke (->ProgramStep - [2 1] 0) rm) =>
      (->RegisterMachine [9 8 7] [-1 5 6] [:foo])

    (invoke (->ProgramStep * [0 3] 1) rm) =>
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
  (random-program-step all-functions 10 12) =>
    (->ProgramStep :FOO [17 17] 7)
  (provided (rand-nth all-functions) => [:FOO 2],
            (rand-int 22) => 17
            (rand-int 12) => 7))


(fact "random-program-step with arity 1"
  (random-program-step all-functions 10 12) =>
    (->ProgramStep rm-not [17] 7)
  (provided (rand-nth all-functions) => [rm-not 1],
            (rand-int 22) => 17
            (rand-int 12) => 7))
