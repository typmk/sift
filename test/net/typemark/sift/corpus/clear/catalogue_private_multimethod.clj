(ns corpus.clear.catalogue-private-multimethod)
(defmulti indenter-fn (fn [_ _ rule] (first rule)))
(defn- helper [x] x)
(def ^:private table {})
