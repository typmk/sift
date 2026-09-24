(ns corpus.flag.catalogue-rt-internal)
(set! *warn-on-reflection* true)
(defn walk [xs] (iterator-seq (clojure.lang.RT/iter xs)))
