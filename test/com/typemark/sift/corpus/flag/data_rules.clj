(ns data-rules)

;; only what splint does not say — see rules.edn's header
(defn g [xs] (first (filter even? xs)))
(defn h [k] (cond (= k :a) 1 (= k :b) 2 :else 3))
(defn j [] (Thread/sleep 100))
(defn m [a x] (swap! a (fn [v] (conj v (count @a) x))))
