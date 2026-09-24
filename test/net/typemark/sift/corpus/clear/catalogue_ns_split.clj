(ns corpus.clear.catalogue-ns-split
  (:require [clojure.string :as str]))
(defn load [x] x)                          ; a local named load is not core/load
(defn go [] (load 1))
(comment (in-ns 'user))                    ; a comment body is not code
(defn j [xs] (str/join "," xs))
