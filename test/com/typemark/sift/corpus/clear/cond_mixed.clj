(ns cond-mixed)

(defn kind [x y]
  (cond
    (= x :circle) "round"
    (= y :square) "boxy"
    :else "unknown"))

(defn range-of [n]
  (cond
    (< n 10) :small
    (= n 10) :ten
    :else :big))

(defn one-and-else [x]
  (cond
    (= x :a) 1
    :else 2))
