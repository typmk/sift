(ns cond-case)

(defn kind [x]
  (cond
    (= x :circle) "round"
    (= x :square) "boxy"
    (= :line x) "thin"
    :else "unknown"))

(defn code [n]
  (cond
    (= n 200) :ok
    (= n 404) :missing))
