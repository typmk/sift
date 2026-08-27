(ns loop-map
  "TRUE POSITIVE. Inverse of (into [] (map inc) xs).")

(defn bump [xs]
  (loop [xs xs out []]
    (if (seq xs)
      (recur (rest xs) (conj out (inc (first xs))))
      out)))
