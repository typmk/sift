(ns corpus.clear.catalogue-let-chain)
(defn transform [xs] (->> xs (map inc) (filter even?) (reduce +)))
;; a binding used TWICE is not a pipeline — threading would duplicate work
(defn branched [xs] (let [a (map inc xs) b (filter even? a) c (remove even? a)] [b c]))
;; a conditional step threads worse than it reads
(defn guarded [x] (let [a (inc x) b (if (pos? a) (dec a) 0) c (* b 2)] c))
;; the body is not the last binding
(defn not-last [xs] (let [a (map inc xs) b (filter even? a) c (reduce + b)] [a c]))
