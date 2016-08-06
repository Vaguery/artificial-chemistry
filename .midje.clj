(change-defaults  :fact-filter #(and
                                  (not (:generational %1))
                                  (not (:lexicase %1))
                                  ))