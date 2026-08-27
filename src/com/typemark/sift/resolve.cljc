(ns com.typemark.sift.resolve
  "What a symbol at a position IS, from clj-kondo's analysis — the one thing
  a text rule cannot know. `(c/atom …)` is `clojure.core/atom` and a `swap!`
  that is a parameter is not a mutator; measured on one fixture, the textual
  rule missed the first and flagged the second.

  Read, not computed: sift never resolves. The analysis arrives as the JSON
  clj-kondo wrote (`--config '{:analysis {:locals true} :output {:format
  :json}}'`), the same input `analysis.cljc` and `dictionary.cljc` already
  take. Without it every rule behaves exactly as before."
  (:require [clojure.string :as str]
            [com.typemark.sift.json :as json]))

(defn index
  "analysis JSON text -> {filename {:vars {[row col] \"ns/name\"}
                                     :locals #{[row col]}}}.
  `:vars` is keyed by the CALL FORM's position — kondo reports the list, not
  the head token; `:locals` by each local usage token's position."
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
  "The entry for `path`, matched by the longest filename suffix — kondo names
  a file however it was invoked, and so does the caller."
  [idx path]
  (when (and idx path)
    (->> (keys idx)
         (filter #(or (str/ends-with? path %) (str/ends-with? % path)))
         (sort-by count >)
         first
         (get idx))))
