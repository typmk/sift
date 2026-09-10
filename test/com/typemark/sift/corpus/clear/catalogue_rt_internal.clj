(ns corpus.clear.catalogue-rt-internal
  (:require [clojure.string :as str]))
(defn j [xs] (str/join "," xs))
(defn other [x] (my.rt/thing x))
