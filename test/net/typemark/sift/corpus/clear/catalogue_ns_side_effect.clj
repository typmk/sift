(ns corpus.clear.catalogue-ns-side-effect
  (:require [clojure.set :as set]))
;; a lazy resolve INSIDE a function is the idiom for breaking a cycle
(defn setting [k] ((requiring-resolve 'my-app.config/get-setting) k))
(defn u [a b] (set/union a b))
(comment (require '[clojure.string :as str]))
