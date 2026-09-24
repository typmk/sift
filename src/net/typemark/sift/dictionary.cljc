(ns net.typemark.sift.dictionary
  "A project's own vocabulary, enforced.

  A codebase that keeps a `banned -> use` table has it enforced by a person
  re-reading it. Every keyword is already in clj-kondo's analysis output --
  namespace, name, position and enclosing var -- so this needs no parsing of
  its own, and the table is the caller's: no upstream analyzer can ship it,
  because the vocabulary is the project's.

  The table is a map from keyword to what to use instead, as EDN:

    {:dimension           \":facet\"
     :party/platform-role \"an off-graph staff fact, never a party attribute\"}

  A bare keyword matches only bare keywords; a qualified one only that
  namespace. Keep generic words out of it: `:err` is clojure.java.shell/sh's
  return key, and a rule that demands renaming another library's contract
  gets switched off."
  (:require [net.typemark.sift.json :as json]
            [clojure.string :as str]))

(defn table
  "`{kw use-instead}` -> `{[ns name] use-instead}`, keyed as clj-kondo
  reports a keyword: namespace nil for a bare one."
  [banned]
  (into {} (for [[k v] banned :let [k (keyword k)]] [[(namespace k) (name k)] (str v)])))

(defn- render [[ns' nm]] (if ns' (str ":" ns' "/" nm) (str ":" nm)))

(defn findings
  "Banned-vocabulary findings, per file, from clj-kondo's analysis output.
  `banned` is the caller's `{kw use-instead}`; nil or empty finds nothing."
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
