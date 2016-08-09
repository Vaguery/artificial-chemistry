(ns artificial-chemistry.linear
  (:use [artificial-chemistry.core]
  			[artificial-chemistry.lexicase])
	(:require [roul.random :as rr]
            [com.climate.claypoole :as cp]))



(defn invoke-ordered-program
  "Takes a RegisterMachine. Runs the program through, in order."
  [rm]
  (reduce (fn [machine step] (invoke step machine)) rm (:program rm)))



(defn output-running-ordered
  "Takes a RegisterMachine, and a vector of inputs. Overwrites the `:connectors` vector of the RegisterMachine with the inputs (starting at the front end) and executes the program IN ORDER. Returns the value of the last element of `:connectors` at the last step."
  [rm inputs]
  (output 
    (invoke-ordered-program (set-inputs rm inputs))))


(defn output-vector-ordered
  "Takes a RegisterMachine, and a training data set. Returns a collection of output values, one for each of the training cases, running the program IN ORDER."
  [rm data]
  (map #(output-running-ordered rm (first %)) data) )



(defn error-vector-ordered
  "Takes a RegisterMachine,  and a training data set. Returns a collection of absolute errors, comparing the output observed for each training case to its expected value. RUNS PROGRAMS IN ORDER."
  [rm data]
  (let [outs (output-vector-ordered rm data)]
    (into [] (map #(Math/abs (- %1 %2)) outs (pmap second data)))
    ))



(defn errors-and-failures-ordered
  "Takes a RegisterMachine, and a bunch of training cases, runs the machine for each case IN ORDER, collects the error-vector, and returns a hash with two scores: the MSE of all numerical results, and the count of non-numerical results"
  [rm data]
  (let [errs (error-vector-ordered rm data)
        bad  (filter isNaN? errs)
        good (filter #(not (isNaN? %)) errs)]
      {:sse (sum-squared-error good) :failures (count bad) :error-vector errs}
    ))



(defn record-errors-ordered
  "Takes a RegisterMachine, and a pile of training cases. Evaluates the machine over each training step RUNNING THE PROGRAM IN ORDER, calculates its `error-vector` and `errors-and-failures` hash, which is returned with the new items pushed onto stacks associated with the "
  [rm data]
  (let [enf (errors-and-failures-ordered rm data)]
    (-> rm
      (push-into-value , :error-vector (:error-vector enf))
      (push-into-value , :sse (:sse enf))
      (push-into-value , :failures (:failures enf)))))




(defn crossover-program-ordered
  "Takes two RegisterMachines. Picks a random point somewhere in each, and exchanges the end of the second one's program for the end of the first one's."
  [mom dad size-limit]
  (let [mom-program  (:program mom)
        mom-cutpoint (max 1 (rand-int (count mom-program)))
        dad-program  (:program dad)
        dad-cutpoint (max 1 (rand-int (count dad-program)))
        baby  (concat
					      (take mom-cutpoint mom-program)
					      (drop dad-cutpoint dad-program))]
	  (into [] (drop (max 0 (- (count baby) size-limit)) baby))
	  ))
    	 	 



(defn crossover-ordered
  "Takes two RegisterMachines, and does crossover on their `:read-only` vectors and programs, using ordered point crossover"
  [mom dad size-limit]
  (->RegisterMachine
    (crossover-registers mom dad)
    (:connectors mom)
    (crossover-program-ordered mom dad size-limit)))



(defn score-pile-ordered
  "Takes a pile of `RegisterMachine` items and a dataset (of training cases). Will score each machine on every entry in the dataset. NOTE: all scores are saved as _stacks_ of scores; each time a RegisterMachine is scored, a new score is _added_ to its stacks."
  [machines dataset]
  (cp/pmap (+ 2 (cp/ncpus)) #(record-errors-ordered % dataset) machines))



(defn breed-one-ordered
  "Takes a collection of RegisterMachines. Returns one new crossover product, produced by sampling two parents with uniform probbaility (with replacement) from the pile. Does not add it back into the pile."
  [pile size-limit]
  (let [mom  (rand-nth pile)
        dad  (rand-nth pile)]
    (crossover-ordered mom dad size-limit)
    ))



(defn tournament-breed-one
	[pile t size-limit mse-weight fail-weight]
	(let [tourney (take (max 2 t) (shuffle pile))
			  [mom dad] (generational-cull-many tourney 2 mse-weight fail-weight)]
	  (crossover-ordered mom dad size-limit)
		))



(defn linearGP-breed-many
  "Takes a collection of RegisterMachines. Returns a collection of N new crossover products"
  [pile size mutation-rate mutation-stdev]
  (repeatedly size #(mutate (breed-one-ordered pile) mutation-rate mutation-stdev)))



(defn tournament-breed-many
  "Takes a collection of RegisterMachines. Returns a collection of N new crossover products"
  [pile t-size size size-limit mutation-rate mutation-stdev mse-weight fail-weight]
  (repeatedly size 
  	#(mutate (tournament-breed-one pile t-size size-limit mse-weight fail-weight) mutation-rate mutation-stdev)))



(defn one-linearGP-step
  "Assumes machines are scored before arriving"
  [pile data t-size size-limit mutation-rate mutation-stdev mse-weight fail-weight]
  (let [n (count pile)
        best (first pile)
        ro (count (:read-only best))
        scale (apply max (:read-only best))
        cxn (count (:connectors best))
        fxn-count (count (:program best))
        ]

    (-> pile
        (into , (tournament-breed-many pile
        					t-size n size-limit mutation-rate mutation-stdev
                  mse-weight fail-weight))
        (score-pile-ordered , data)
        (generational-cull-many , n 1 1e12)
        )))


(defn linearGP-search
  "Starts a long-running loop of generations, for the specified problem set, and starting with the specified pile of individuals. In each generation, the pile is increased by the indicated factor, mutation is applied, and data files are saved. Old data files are overwritten."
  [dataset dataname
    pop-size ro-count constant-scale connector-count function-count function-set 
    t-size size-limit generations
    mse-weight failure-weight mutation-rate mutant-stdev]

  (let [start-pile (starting-pile pop-size
                                  ro-count 
                                  constant-scale 
                                  connector-count
                                  function-count
                                  function-set)
        start-pile (score-pile-ordered start-pile dataset)]

      (spit (str "linearGP-rms-" dataname ".csv") "")

      (loop [pile (score-pile-ordered start-pile dataset)
             step 0]
        (report-line (str "linearGP-rms-" dataname ".csv") step pile)
        (if (or (> step generations) (< (apply max (:sse (first pile))) 0.0001))
          (report-best (str "linearGP-rms-" dataname "-best.csv") step pile)
          (do (println (class pile))
          (recur (one-linearGP-step 
                      pile dataset t-size size-limit mutation-rate mutant-stdev mse-weight failure-weight)
                   (inc step))
                   )))))
