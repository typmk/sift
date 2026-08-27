(ns idiomatic
  "Control A. House style. Must produce zero findings."
  (:require [clojure.string :as str]))

(defn- request->line [{:keys [supplier product qty reason]}]
  {:product product :qty qty :reason (or reason "unspecified") :supplier supplier})

(defn summarise
  [requests]
  (->> requests
       (filter :supplier)
       (map request->line)
       (group-by :supplier)))

(defn describe
  [summary]
  (->> summary
       (sort-by (comp - count val))
       (map (fn [[supplier lines]]
              (str supplier ": " (count lines) " lines, "
                   (reduce + (map :qty lines)) " units")))
       (str/join "\n")))
