(ns loop-bound-atom
  "TRUE POSITIVE. Atom bound on the loop, not a let.")

(defn collect [xs]
  (loop [acc (atom []) xs xs]
    (if (seq xs)
      (do (swap! acc conj (first xs))
          (recur acc (rest xs)))
      @acc)))
