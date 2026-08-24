(ns com.typemark.sift.dictionary
  "The project's own vocabulary, enforced.

  CLAUDE.md opens with `The enemy of progress is inconsistency` and carries a
  `Banned -> use` table: dimension becomes facet, slot becomes :taxon/factor,
  custody becomes controller, visibility becomes scope. Until now that table
  was enforced by a human re-reading it.

  Every keyword in the codebase is already in clj-kondo's analysis output --
  namespace, name, position and enclosing var -- so this needs no parsing of
  its own. It consumes what the symbol table already reads.

  This is the one rule set no upstream analyzer will ever ship, because the
  vocabulary is this organisation's."
  (:require [com.typemark.sift.json :as json]
            [clojure.string :as str]))

(def banned
  "Banned keyword -> what to use instead, keyed as clj-kondo reports it:
  [namespace name], namespace nil for a bare keyword.

  DELIBERATELY NARROWER than CLAUDE.md's full table. Measured against lume,
  sur, forma and scout, the generic entries fired on keys this codebase does
  not own: `:err` is clojure.java.shell/sh's return shape, alongside `:exit`
  and `:out`. You cannot rename another library's contract, and a rule that
  demands it gets switched off.

  What remains are terms distinctive enough that a match is this project's own
  vocabulary. The generic ones are listed in `ambiguous` below, off by design."
  {[nil "dimension"]           ":facet"
   [nil "vocab"]               ":option"
   [nil "vocabulary"]          ":option"
   [nil "slot"]                ":taxon/factor or a facet"
   ["taxon" "require"]         ":taxon/factor"
   [nil "custody"]             ":controller, :operator or :key-holder -- a folk word across three axes"
   ["party" "platform-role"]   "an off-graph staff fact; operator access is never a party attribute"
   [nil "visibility"]          ":scope"
   [nil "flavour"]             "the concrete term"
   [nil "test-mode"]           ":target -- stripe-test IS the rehearsal; one axis, not two"})

(def ambiguous
  "Banned by CLAUDE.md, but too common in third-party APIs to enforce on a
  keyword alone. Recorded so the omission is deliberate and visible rather
  than an oversight.

  `:audience` is the sharpest case: the Banned table says audience -> scope,
  while the Access section defines the access kernel AS `audience +
  sensitivity + capability` and glosses scope as `audience (who may see)`.
  The dictionary contradicts itself there, and that is a question for the
  dictionary, not a rule."
  {[nil "err"]        ":fault  -- but also clojure.java.shell/sh's return key"
   [nil "failure"]    ":fault"
   [nil "entry"]      ":event  -- but also a common map/zip/tar key"
   [nil "element"]    ":field, :input or :form"
   [nil "processor"]  ":operator"
   [nil "audience"]   ":scope  -- contradicted by the Access section"
   [nil "requires"]   ":taxon/factor -- too generic bare"})

(defn- render [[ns' nm]] (if ns' (str ":" ns' "/" nm) (str ":" nm)))

(defn findings
  "Banned-vocabulary findings, per file, from clj-kondo's analysis output."
  [analysis-text]
  (let [ks (get-in (json/read-str analysis-text) ["analysis" "keywords"] [])]
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
