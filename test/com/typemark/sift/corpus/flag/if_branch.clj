(ns if-branch
  "TRUE POSITIVE. Place-as-fold, but the else is not the acc — no counterpart.")

(defn collect [xs]
  (let [acc (atom {})]
    (doseq [x xs]
      (if (:k x)
        (swap! acc update :k (fnil conj []) x)
        (println x)))
    @acc))
