(ns allow-meta
  "HARD NEGATIVE. The allow tag is a greppable override.")

(defn accumulate [xs]
  (let [^:places/allow acc (atom [])]
    (doseq [x xs]
      (swap! acc conj x))
    @acc))
