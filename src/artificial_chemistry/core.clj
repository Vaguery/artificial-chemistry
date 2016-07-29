(ns artificial-chemistry.core
   (:require [clojure.math.numeric-tower :as math]))


(defrecord RegisterMachine [read-only connectors program])


(defn value-of
  "Takes a RegisterMachine record and index. The index refers
  to the index in the _concatenated_ `:read-only` and `connectors`
  vectors. Returns the value stored there."
  [rm idx]
  (nth (concat (:read-only rm) (:connectors rm)) idx))



(defn write-value
  "Takes a RegisterMachine record, index, and new value.
  Updates the value stored in the `:connectors` vector at
  that index."
  [rm idx number]
  (assoc-in rm [:connectors idx] number))



(defrecord ProgramStep [function args target])



(defn pdiv
  "Protected division; returns 1.0 when division by zero is attempted"
  [dividend divisor]
  (if (zero? divisor)
    1.0
    (/ dividend divisor)))



(defn pow
  "Exponentiation"
  [base exponent]
  (math/expt base exponent))



(defn rm-not
  "logical not on a numerical value"
  [n]
  (if (zero? n) 1.0 0.0))



(defn rm-and
  "logical `and` on numerical values"
  [n1 n2]
  (let [n1z (not (zero? n1))
        n2z (not (zero? n2))]
  (if (and n1z n2z) 1.0 0.0)
  ))



(defn rm-or
  "logical `or` on numerical values"
  [n1 n2]
  (let [n1z (not (zero? n1))
        n2z (not (zero? n2))]
  (if (or n1z n2z) 1.0 0.0)
  ))



(def all-functions
  [ [+' 2],  [*' 2],     [-' 2],    [pdiv 2],
    [pow 2], [rm-and 2], [rm-or 2], [rm-not 1]])



(defn random-program-step
  [functions readonly connectors]
  (let [readable (+ readonly connectors)
       [which-fxn arity] (rand-nth all-functions)]
  (->ProgramStep
    which-fxn
    (into [] (take arity (repeatedly #(rand-int readable))))
    (rand-int connectors)
    )
  ))


(defn invoke
  [ps machine]
  (let [indices (into [] (concat (:read-only machine) (:connectors machine)))
        values  (into [] (map indices (:args ps)))
        result  (apply (:function ps) values)]
    (assoc-in machine [:connectors (:target ps)] result)
    ))


(defn random-program
  [functions readonly connectors steps]
  (take steps 
    (repeatedly #(random-program-step functions readonly connectors))))


(defn invoke-any-step
  "Takes a RegisterMachine, picks a random ProgramStep from its `program`, and `invoke`s that step on the machine"
  [rm]
  (invoke (rand-nth (:program rm)) rm))


(defn invoke-many-steps
  "Takes a RegisterMachine and a number of iterations. In each iteration, it applies `invoke-any-step`"
  [rm steps]
  (nth (iterate invoke-any-step rm) steps))


(defn rm-trace
  "Takes a RegisterMachine and a number of iterations. It returns the lazy sequence of all the `:connectors` registers, recorded once for each step of iteration."
  [rm steps]
  (map :connectors (take steps (iterate invoke-any-step rm))))


(defn output
  "Returns the current value of the last `:connectors` element of an RegisterMachine"
  [rm]
  (last (:connectors rm)))


(defn set-inputs
  "Takes a RegisterMachine and a vector of input values. Replaces the first portion of the `:connectors` vector with these values."
  [rm inputs]
  (let [i (count inputs)]
    (assoc rm 
           :connectors 
           (into [] (concat inputs (drop i (:connectors rm)))))))


(defn output-given-inputs
  "Takes a RegisterMachine, a count of steps to run it, and a vector of inputs. Overwrites the `:connectors` vector of the RegisterMachine with the inputs (starting at the front end) and executes the program steps at random. Returns the value of the last element of `:connectors` at the last step."
  [rm steps inputs]
  (output 
    (invoke-many-steps (set-inputs rm inputs) steps)))


(defn random-sine-case
  []
  (let [x (- (* 2 (rand Math/PI)) Math/PI)]
    [[x] (Math/sin x)]
    ))


(def sine-data
  (repeatedly 100 random-sine-case))



(defn output-vector
  "Takes a RegisterMachine, a number of steps, and a training data set. Returns a collection of output values, one for each of the training cases."
  [rm steps data]
  (map #(output-given-inputs rm steps (first %)) data) )



(defn error-vector
  "Takes a RegisterMachine, a number of steps, and a training data set. Returns a collection of absolute errors, comparing the output observed for each training case to its expected value."
  [rm steps data]
  (let [outs (output-vector rm steps data)]
    (map #(Math/abs (- %1 %2)) outs (map second data))
    ))
