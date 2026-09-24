(ns atom-callback
  "HARD NEGATIVE. The fn is bound and called later — a callback place, not a fold.")

(defn collect [xs]
  (let [acc (atom [])
        add! (fn [x] (swap! acc conj x))]
    (doseq [x xs]
      (add! x))
    @acc))
