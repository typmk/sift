(ns loop-filter-map
  "TRUE POSITIVE. Inverse of (into [] (comp (filter even?) (map inc)) xs).")

(defn bump-evens [xs]
  (loop [xs xs out []]
    (if (seq xs)
      (let [x (first xs)]
        (recur (rest xs) (if (even? x) (conj out (inc x)) out)))
      out)))
