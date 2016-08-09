(ns artificial-chemistry.core
   (:require [clojure.math.numeric-tower :as math]
             [roul.random :as rr]
             [com.climate.claypoole :as cp]))


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



(defrecord ProgramStep [name function args target])



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


(defn rm-copy
  "copy item from one register to another"
  [n1] 
  n1)



(def all-functions
  { :add  [+       2]
    :mul  [*       2]
    :sub  [-       2]
    :pdiv [pdiv    2]
    :pow  [pow     2]
    :and  [rm-and  2]
    :or   [rm-or   2]
    :not  [rm-not  1]
    :copy [rm-copy 1]
    })



(defn random-program-step
  "Constructs a new ProgramStep record, selecting a random function (uniformly) from the hash provided, and :args and :target values (also uniformly) from the range of `readonly` and `connectors` provided."
  [functions readonly connectors]
  (let [readable (+ readonly connectors)
       [fn-name  [which-fxn arity]] (rand-nth (sort (seq all-functions)))]
  (->ProgramStep
    fn-name
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



(defn sum-squared-error
  "Takes a collection of numeric values, squares them, adds those to produce a single sum"
  [numbers]
  (reduce + (map #(* %1 %1) numbers)))



(defn isNaN?
  [number]
  (Double/isNaN number))



(defn errors-and-failures
  "Takes a RegisterMachine, a repeat scale and a bunch of training cases, runs the machine for each case (scale times for each program step it contains), collects the error-vector, and returns a hash with two scores: the MSE of all numerical results, and the count of non-numerical results"
  [rm scale data]
  (let [errs (error-vector rm (* scale (count (:program rm))) data)
        bad  (filter isNaN? errs)
        good (filter #(not (isNaN? %)) errs)]
      {:sse (sum-squared-error good) :failures (count bad) :error-vector errs}
    ))



(defn push-into-value
  "Takes a RegisterMachine, a keyword and an item. If the keyword does not exist, it is first created and an empty vector is assigned its value. Then the item is pushed onto that vector. If the vector already exists, the item is simply pushed."
  [rm kw item]
  (let [old (get rm kw [])]
    (assoc rm kw (conj old item))
    ))


(defn record-errors
  "Takes a RegisterMachine, scale factor for running, and a pile of training cases. Evaluates the machine over each training step, calculates its `error-vector` and `errors-and-failures` hash, which is returned with the new items pushed onto stacks associated with the "
  [rm scale data]
  (let [enf (errors-and-failures rm scale data)]
    (-> rm
      (push-into-value , :error-vector (:error-vector enf))
      (push-into-value , :sse (:sse enf))
      (push-into-value , :failures (:failures enf)))))



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
  [rm prob stdev]
  (let [old-registers (:read-only rm)]
    (assoc rm 
           :read-only
           (map #(if (< (rand) prob) (rr/rand-gaussian % stdev) %) old-registers))
    ))


(defn crossover
  "Takes two RegisterMachines, and does crossover of both their `:read-only` vectors and programs"
  [mom dad]
  (->RegisterMachine
    (crossover-registers mom dad)
    (:connectors mom)
    (crossover-program mom dad)))



(defn mutate
  "Takes a RegisterMachine and a probability and a standard deviation of the change, and changes both the `:read-only` registers and the program steps with that probability."
  [rm prob stdev]
  (-> rm
      (mutate-registers , prob stdev)
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



(defn starting-pile
  "Creates a collection of specified size, of random RegisterMachine instances with the specified number of read-only "
  [size read-only constant-scale connectors functions function-set]
  (into [] (repeatedly size 
    #(randomize-read-only
      (random-register-machine function-set read-only connectors functions)
        constant-scale))))




(defn score-pile
  "Takes a pile of `RegisterMachine` items and a dataset (of training cases). Will score each machine on every entry in the dataset, using the indicated number of samples per function in the programs. For example, if the scaling-factor is 5 and a machine being scored has 100 functions, there will be 500 samples run in generating the score. NOTE: all scores are saved as _stacks_ of scores; each time a RegisterMachine is scored, a new score is _added_ to its stacks."
  [machines scaling-factor dataset]
  (cp/pmap (+ 2 (cp/ncpus)) #(record-errors % scaling-factor (take 20 (shuffle dataset))) machines))



(defn breed-one
  "Takes a collection of RegisterMachines. Returns one new crossover product, produced by sampling two parents with uniform probbaility (with replacement) from the pile. Does not add it back into the pile."
  [pile]
  (let [mom  (rand-nth pile)
        dad  (rand-nth pile)]
    (crossover mom dad)
    ))


(defn cull-one
  "Takes a collection of RegisterMachines, and relative weights for MSE and failures scores. Returns the same collection, lacking one of the machines with the lowest weighted sum of (max :sse) and (max :failures)"
  [pile mse-weight fail-weight]
  (butlast 
    (sort-by
      #(+ (* mse-weight (apply max (:sse %))) 
          (* fail-weight (apply max (:failures %))))
     (shuffle pile))))



(defn one-steady-state-step
  "Assumes the machines are scored before arriving"
  [pile scale data]
  (let [baby (record-errors (breed-one pile) scale data)
        mute (record-errors (mutate (rand-nth pile) 0.03 1.0) scale data)]
    (-> pile
        cull-one
        (conj , mute)
        cull-one
        (conj , baby)
    )))




(defn report-line
  [filename t pile]
  (do 
    (spit filename
          (str t ", "
            (clojure.string/join ", " (map #(apply max (:sse %)) pile)) ", "
            (clojure.string/join ", " (map #(count (:program %)) pile))
            "\n")
          :append true)
    (println
      (str t ", "
        (clojure.string/join ", " (take 5 (map #(apply max (:sse %)) pile))) "..."
        ))
    ))


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
  [pile size mutation-rate mutation-stdev]
  (repeatedly size #(mutate (breed-one pile) mutation-rate mutation-stdev)))



(defn generational-cull-many
  [pile keep mse-weight fail-weight]
  (take keep 
    (sort-by
      #(+ (* mse-weight (apply max (:sse %))) 
          (* fail-weight (apply max (:failures %))))
     (shuffle pile))))



(defn one-generational-step
  "Assumes machines are scored before arriving; shuffles and samples data for each evaluation"
  [pile scale-factor data mutation-rate mutation-stdev mse-weight fail-weight]
  (let [n (count pile)
        best (first pile)
        ro (count (:read-only best))
        scale (apply max (:read-only best))
        cxn (count (:connectors best))
        fxn-count (count (:program best))
        ]

    (into []
      (-> pile
        (into , (starting-pile n ro scale cxn (+ fxn-count (rand-int fxn-count)) all-functions))
        (into , (generational-breed-many pile n mutation-rate mutation-stdev))
        (score-pile , scale-factor data)
        (score-pile , scale-factor data)
        (score-pile , scale-factor data)
        (score-pile , scale-factor data)
        (score-pile , scale-factor data)
        (generational-cull-many , n mse-weight fail-weight)
        ))))



(defn multiple-score-samples
  "Takes a RegisterMachine, a scale factor, a dataset, and a number of replications to sample. Returns the `:sse` measured for each replication, on each of the training cases, for just that machine."
  [rm scale-factor data replicates]
  (take replicates 
    (iterate #(record-errors % scale-factor data) rm)))


(defn report-consistency
  "Takes a pile, picks the first machine, and runs it N times, printing the measured MSE"
  [pile scale-factor data replicates]
  (println "  consistency: "
    (clojure.string/join ", " 
      (map :sse (multiple-score-samples (first pile) scale-factor data replicates)))))



(defn generational-search
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

      (spit (str "generational-rms-" dataname ".csv") "")

      (loop [pile start-pile
             step 0]
        (report-line (str "generational-rms-" dataname ".csv") step pile)
        (if (or (> step generations) (< (apply max (:sse (first pile))) 0.0001))
          (report-best (str "generational-rms-" dataname "-best.csv") step pile)
          (recur (one-generational-step 
                      pile sampling-factor dataset mutation-rate mutant-stdev
                      mse-weight failure-weight)
                   (inc step))
                   ))))



