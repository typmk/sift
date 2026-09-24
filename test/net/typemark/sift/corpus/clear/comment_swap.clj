(ns comment-swap
  "HARD NEGATIVE. The only swap!s are discarded.")

(defn empty-fold [xs]
  (let [acc (atom [])]
    #_(swap! acc conj :no)
    (comment (swap! acc conj :no))
    @acc))
