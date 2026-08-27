(ns com.typemark.sift.metrics
  "Size and complexity measures over the parsed tree.

  Every Sonar ratio -- comment density, duplication density, technical-debt
  ratio -- divides by ncloc. Without these the dashboard reports a project
  with no code and every ratio is undefined."
  (:require [clojure.string :as str]
            [com.typemark.sift.complexity :as complexity]
            [com.typemark.sift.parse :as parse]))

(defn- line-span [{:keys [line end-line]}] (range line (inc end-line)))

(defn from-nodes
  "Measures over an already-parsed node stream, so a caller needing the nodes
  for other purposes parses once."
  [nodes]
  (let [leaves    (parse/leaves nodes)
        code      (into #{} (comp (remove :commented?)
                                  (remove #(contains? #{:comment :trivia} (:type %)))
                                  (mapcat line-span))
                        leaves)
        commented (into #{} (comp (filter #(or (:commented? %) (= :comment (:type %))))
                                  (remove #(= :trivia (:type %)))
                                  (mapcat line-span))
                        leaves)
        live      (remove :commented? nodes)
        branches  (filter :branch? live)]
    {:ncloc         (count code)
     :comment-lines (count (remove code commented))
     :functions     (count (filter :function? live))
     :classes       (count (filter :class? live))
     :statements    (count (filter #(= :list (:tag %)) live))
     :complexity    (inc (count branches))
     :cognitive     (reduce + 0 (map #(inc (:branch-nesting %)) branches))}))

(defn- line-map
  "Sonar's per-line data format: \"1=1;2=0;3=1\"."
  [lines all]
  (->> (sort all)
       (map #(str % "=" (if (contains? lines %) 1 0)))
       (str/join ";")))

(defn line-data
  "The two per-line maps Sonar needs to reason about *which* lines matter.

  `ncloc-data` marks code lines; `executable-data` marks lines that could
  have been covered. New-code coverage is computed against the executable
  set.

  `truth`, when given, is the set of lines the coverage tool actually
  instrumented, and is used verbatim. The heuristic below is only a fallback
  for files no coverage report mentions: measured against cloverage on this
  project it misses 17% of instrumented lines, so it is a guess and the
  report is not."
  ([nodes] (line-data nodes nil))
  ([nodes truth]
  (let [leaves (parse/leaves nodes)
        code   (into #{} (comp (remove :commented?)
                               (remove #(contains? #{:comment :trivia} (:type %)))
                               (mapcat line-span))
                     leaves)
        exec   (into #{} (comp (remove :commented?)
                               (filter #(= :list (:tag %)))
                               (map :line))
                     nodes)
        exec   (or truth exec)
        span   (into code exec)]
    {:ncloc-data      (line-map code span)
     :executable-data (line-map exec span)})))

(defn with-complexity
  "`:complexity` and `:cognitive` from the unit engine — the sum over every
  unit of what `complexity.cljc` scored, which is what Sonar means by a
  file's complexity — replacing the node-stream count that `from-nodes`
  carries. `from-nodes` kept its own because it had only nodes; given the
  text, one engine scores both the file and the units."
  [m source path]
  (let [{:keys [ok? functions]} (complexity/report source path)
        units (when ok? (complexity/flatten-units functions))]
    (if ok?
      (assoc m :complexity (reduce + 1 (map #(dec (:cyclomatic % 1)) units))
               :cognitive (reduce + 0 (map :cognitive units)))
      m)))

(defn measures
  "Measures for a source string, or nil when it does not parse."
  ([source] (measures source nil))
  ([source path]
   (let [{:keys [ok? nodes]} (parse/parse source)]
     (when ok? (with-complexity (from-nodes nodes) source path)))))
