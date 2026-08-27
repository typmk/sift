(ns named-fn-step
  "TRUE POSITIVE. A named fn passed to run! is still a fold.")

(defn collect [xs]
  (let [acc (atom [])]
    (run! (fn step [x] (swap! acc conj x)) xs)
    @acc))
