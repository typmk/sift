(ns net.typemark.sift.resolve
  (:require [clojure.string :as str]
            [net.typemark.sift.json :as json]))

(defn index
  [analysis-text]
  (let [a (get (json/read-str analysis-text) "analysis")]
    (reduce (fn [m [f k v]] (update-in m [f k] (fnil into (if (= k :vars) {} #{})) [v]))
            {}
            (concat
             (for [u (get a "var-usages" [])
                   :when (and (get u "to") (get u "row") (get u "col"))]
               [(get u "filename") :vars
                [[(get u "row") (get u "col")] (str (get u "to") "/" (get u "name"))]])
             (for [l (get a "local-usages" [])
                   :when (and (get l "row") (get l "col"))]
               [(get l "filename") :locals [(get l "row") (get l "col")]])))))

(defn for-file
  [idx path]
  (when (and idx path)
    (->> (keys idx)
         (filter #(or (str/ends-with? path %) (str/ends-with? % path)))
         (sort-by count >)
         first
         (get idx))))
