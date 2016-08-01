(ns artificial-chemistry.core
   (:require [clojure.math.numeric-tower :as math]
             [roul.random :as rr]))


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
    (into [] (map #(Math/abs (- %1 %2)) outs (pmap second data)))
    ))


(defn mean-squared-error
  "Takes a collection of numeric values, squares them, adds those to produce a single sum"
  [numbers]
  (reduce + (map #(* %1 %1) numbers)))


(defn isNaN?
  [number]
  (Double/isNaN number))


(defn errors-and-failures
  "Takes a RegisterMachine and number of steps to run it, and a bunch of training cases, runs the machine for each case, collects the error-vector, and returns two scores: the MSE of all numerical results, and the count of non-numerical results"
  [rm steps data]
  (let [errs (error-vector rm steps data)
        bad  (filter isNaN? errs)
        good (filter #(not (isNaN? %)) errs)]
      {:mse (mean-squared-error good) :failures (count bad) :error-vector errs}
    ))


(defn record-errors
  "Takes a RegisterMachine, a number of steps to run it, and a pile of training cases. Evaluates the machine over each training step, calcluates its `error-vector` and `errors-and-failures` hash, and associates those into the RegisterMachine, which is returned"
  [rm steps data]
  (let [enf (errors-and-failures rm steps data)]
    (-> rm
      (assoc , :error-vector (:error-vector enf))
      (assoc , :mse (:mse enf))
      (assoc , :failures (:failures enf)))))


(defn crossover-program
  "Takes two RegisterMachines. Randomly samples some ProgramSteps from each, returning a new program combining the samples"
  [mom dad]
  (let [mom-program (:program mom)
        mom-contrib (max 1 (rand-int (count mom-program)))
        dad-program (:program dad)
        dad-contrib (max 1 (rand-int (count dad-program)))]
    (into [] (concat
      (repeatedly mom-contrib #(rand-nth mom-program))
      (repeatedly dad-contrib #(rand-nth dad-program))
      ))))



(defn mutate-program
  "Tkes a RegisterMachine, and a probability of mutation. Each step of the `:program` is changed with that probability to a completely new `random-program-step` result."
  [rm prob]
  (let [old-program (:program rm)
        ro-count    (count (:read-only rm))
        cxn-count   (count (:connectors rm))]
    (assoc rm
           :program
           (map #(if (< (rand) prob) 
                  (random-program-step all-functions ro-count cxn-count) 
                  %) 
                old-program))
                ))


(defn crossover-registers
  "Takes two RegisterMachines. Constructs a new `:read-only` vector by sampling from the two with equal probability."
  [mom dad]
  (map #(rand-nth [%1 %2]) (:read-only mom) (:read-only dad)))


(defn mutate-registers
  "Takes one RegisterMachine, and a probability of mutation. With specified probability it replaces each entry in `:read-only` with a gaussian deviate with mean equal to the original value, and standard deviation equal to 1.0"
  [rm prob]
  (let [old-registers (:read-only rm)]
    (assoc rm 
           :read-only
           (map #(if (< (rand) prob) (rr/rand-gaussian % 1.0) %) old-registers))
    ))


(defn crossover
  "Takes two RegisterMachines, and does crossover of both their `:read-only` vectors and programs"
  [mom dad]
  (->RegisterMachine
    (crossover-registers mom dad)
    (:connectors mom)
    (crossover-program mom dad)))


(defn mutate
  "Takes a RegisterMachine and a probability, and changes both the `:read-only` registers and the program steps with that probability."
  [rm prob]
  (-> rm
      (mutate-registers , prob)
      (mutate-program , prob)))


(defn random-register-machine
  "Takes a function set, and counts of `:read-only` and `connectors` registers, and a number of steps in its `:program`. All registers are set to 0.0"
  [function-set read-only connectors steps]
  (->RegisterMachine
    (into [] (repeat read-only 0.0))
    (into [] (repeat connectors 0.0))
    (random-program function-set read-only connectors steps)
    ))


(defn randomize-read-only
  "Takes a RegisterMachine, and a range value, and sets each of the `:read-only` registers to a uniform random value between [-range, range]"
  [rm scale]
  (let [how-many      (count (:read-only rm))
        double-range  (* 2.0 scale)]
    (assoc rm
           :read-only
           (into [] (repeatedly how-many #(- (* (rand) double-range) scale)))
           )))


