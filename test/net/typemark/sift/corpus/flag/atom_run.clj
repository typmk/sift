(ns atom-run
  "TRUE POSITIVE. run! over an anonymous fn is doseq in another hat.")

(defn collect [xs]
  (let [acc (atom [])]
    (run! #(swap! acc conj %) xs)
    @acc))
