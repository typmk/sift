(ns corpus.clear.catalogue-cond-build)
(defn build [input]
  (cond-> {} (:a input) (assoc :a 1) (:b input) (assoc :b 2) (:c input) (assoc :c 3)))
;; two rebindings is a shadow, not a build-up
(defn small [input] (let [m {} m (if (:a input) (assoc m :a 1) m)] m))
