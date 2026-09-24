(ns corpus.flag.catalogue-let-chain)
(defn transform [xs]
  (let [step1 (map inc xs)
        step2 (filter even? step1)
        step3 (reduce + step2)]
    step3))
