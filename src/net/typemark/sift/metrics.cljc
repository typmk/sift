(ns net.typemark.sift.metrics
  (:require [clojure.string :as str]
            [net.typemark.sift.complexity :as complexity]
            [net.typemark.sift.parse :as parse]))

(defn- line-span [{:keys [line end-line]}] (range line (inc end-line)))

(defn from-nodes
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
  [lines all]
  (->> (sort all)
       (map #(str % "=" (if (contains? lines %) 1 0)))
       (str/join ";")))

(defn line-data
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
  [m source path]
  (let [{:keys [ok? functions]} (complexity/report source path)
        units (when ok? (complexity/flatten-units functions))]
    (if ok?
      (assoc m :complexity (reduce + 1 (map #(dec (:cyclomatic % 1)) units))
               :cognitive (reduce + 0 (map :cognitive units)))
      m)))

(defn measures
  ([source] (measures source nil))
  ([source path]
   (let [{:keys [ok? nodes]} (parse/parse source)]
     (when ok? (with-complexity (from-nodes nodes) source path)))))
