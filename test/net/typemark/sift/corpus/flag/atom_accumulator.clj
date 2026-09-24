(ns atom-accumulator
  "TRUE POSITIVE. atom born in the let, mutated in a doseq, deref'd in tail.")

(defn aggregate [requests]
  (let [acc (atom {})]
    (doseq [r requests]
      (swap! acc update (:supplier r) (fnil conj []) r))
    @acc))
