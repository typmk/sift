(ns loop-reduce-not)

(defn pairs [xs]
  (loop [xs xs acc []]
    (if (seq xs)
      (recur (drop 2 xs) (conj acc [(first xs) (second xs)]))
      acc)))

(defn bfs [start neighbours]
  (loop [queue [start] seen #{}]
    (if (seq queue)
      (let [n (first queue)]
        (recur (into (rest queue) (remove seen (neighbours n))) (conj seen n)))
      seen)))

(defn count-then-return [xs]
  (loop [xs xs n 0]
    (if (seq xs)
      (recur (rest xs) (inc n))
      (str n " items"))))

(defn three-bindings [xs]
  (loop [xs xs acc 0 i 0]
    (if (seq xs)
      (recur (rest xs) (+ acc (first xs)) (inc i))
      acc)))
