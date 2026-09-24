(ns net.typemark.sift.dictionary
  (:require [net.typemark.sift.json :as json]
            [clojure.string :as str]))

(defn table
  [banned]
  (into {} (for [[k v] banned :let [k (keyword k)]] [[(namespace k) (name k)] (str v)])))

(defn- render [[ns' nm]] (if ns' (str ":" ns' "/" nm) (str ":" nm)))

(defn findings
  [analysis-text banned]
  (let [banned (table banned)
        ks (when (seq banned) (get-in (json/read-str analysis-text) ["analysis" "keywords"] []))]
    (reduce
     (fn [acc k]
       (let [id [(get k "ns") (get k "name")]]
         (if-let [use-instead (get banned id)]
           (update acc (get k "filename") (fnil conj [])
                   {:rule "banned-term"
                    :line (get k "row") :col (get k "col")
                    :end-line (get k "end-row") :end-col (get k "end-col")
                    :message (str (render id) " is banned by the project dictionary; use "
                                  use-instead)})
           acc)))
     {} ks)))

(defn summary [by-file]
  {:files (count by-file)
   :findings (reduce + 0 (map count (vals by-file)))
   :terms (->> (vals by-file) (mapcat identity) (map :message)
               (map #(first (str/split % #" "))) distinct sort vec)})
