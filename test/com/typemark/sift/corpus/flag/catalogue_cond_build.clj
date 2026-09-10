(ns corpus.flag.catalogue-cond-build)
(defn build [input]
  (let [m {}
        m (if (:a input) (assoc m :a 1) m)
        m (if (:b input) (assoc m :b 2) m)
        m (if (:c input) (assoc m :c 3) m)]
    m))
