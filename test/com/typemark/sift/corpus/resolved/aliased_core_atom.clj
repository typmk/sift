(ns aliased-core-atom
  (:require [clojure.core :as c]))

(defn summarise [xs]
  (let [acc (c/atom [])]
    (doseq [x xs]
      (c/swap! acc conj (inc x)))
    @acc))

(defn shadowed [xs swap!]
  (let [acc (atom [])]
    (doseq [x xs]
      (swap! acc conj x))
    @acc))
