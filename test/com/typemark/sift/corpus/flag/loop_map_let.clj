(ns loop-map-let
  "TRUE POSITIVE. Inverse of (into [] (map inc) xs), with a bound element.")

(defn bump [xs]
  (loop [xs xs out []]
    (if (empty? xs)
      out
      (let [x (first xs)]
        (recur (next xs) (conj out (inc x)))))))
