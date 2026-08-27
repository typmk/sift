(ns aliased-atom
  "HARD NEGATIVE. m/atom is not clojure.core/atom.")

(defn collect [xs]
  (let [acc (m/atom [])]
    (doseq [x xs]
      (m/swap! acc conj x))
    @acc))
