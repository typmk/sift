(ns loop-reduce)

(defn total [nums]
  (loop [xs nums acc 0]
    (if (seq xs)
      (recur (rest xs) (+ acc (first xs)))
      acc)))

(defn index-by-id [rows]
  (loop [acc {} rs rows]
    (if (empty? rs)
      acc
      (let [r (first rs)]
        (recur (assoc acc (:id r) r) (next rs))))))

(defn longest [words]
  (loop [ws words best nil]
    (if-not (seq ws)
      best
      (recur (rest ws) (if (> (count (first ws)) (count best)) (first ws) best)))))
