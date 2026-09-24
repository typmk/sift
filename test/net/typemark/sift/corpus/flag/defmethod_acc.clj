(ns defmethod-acc
  "TRUE POSITIVE. A fold inside defmethod must be visible.")

(defmulti handle :k)

(defmethod handle :x [m]
  (let [acc (atom [])]
    (doseq [x (:xs m)]
      (swap! acc conj x))
    @acc))
