(ns corpus.clear.catalogue-case-else)
(defn orient [r] (case r "portrait" :p "landscape" :l :default))
(defn pick [x] (cond (= x 1) :one :else :other))
