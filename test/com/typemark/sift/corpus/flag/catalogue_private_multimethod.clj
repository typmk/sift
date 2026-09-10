(ns corpus.flag.catalogue-private-multimethod)
(defmulti ^:private indenter-fn (fn [_ _ rule] (first rule)))
