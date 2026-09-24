(ns plain-accumulator
  "TRUE POSITIVE. Twin of allow_meta without the tag — must flag.")

(defn accumulate [xs]
  (let [acc (atom [])]
    (doseq [x xs]
      (swap! acc conj x))
    @acc))
