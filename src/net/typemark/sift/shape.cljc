(ns net.typemark.sift.shape
  (:require [net.typemark.sift.cond-build :as cond-build]
            [net.typemark.sift.let-chain :as let-chain]
            [net.typemark.sift.cond-case :as cond-case]
            [net.typemark.sift.data :as data]
            [net.typemark.sift.fold :as fold]
            [net.typemark.sift.host :as host]
            [net.typemark.sift.loop-fold :as loop-fold]
            [net.typemark.sift.map-loop :as map-loop]
            [net.typemark.sift.resolve :as resolve]
            [net.typemark.sift.typeflow :as typeflow]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.zip :as z]))

(def rules
  (merge
  {fold/rule      {:category :refactor :instruction fold/instruction :evidence :corpus :note "17/17 with the standalone CLI on a code-graph tool"}
   map-loop/rule  {:category :refactor :instruction map-loop/instruction :evidence :corpus}
   loop-fold/rule {:category :refactor :instruction loop-fold/instruction :evidence :corpus :note "0 on hand-written code, 1 in clojure.core — fires on generated code"}
   cond-case/rule {:category :refactor :instruction cond-case/instruction :evidence :corpus :note "1 on a code-graph tool, 7 in clojure.core, all read"}
   cond-build/rule {:category :refactor :instruction cond-build/instruction :evidence :corpus
                    :note "8 of 12,248 let forms over 1,131 files; clojure.core's own defn rebuilds fdecl four times"}
   let-chain/rule  {:category :refactor :instruction let-chain/instruction :evidence :corpus
                    :note "13 of 12,248 let forms; 18 before refusing a conditional step and a qualified let — p/let is promise sequencing, not a chain"}
   :catch-all-swallow     (assoc (:catch-all-swallow host/rules) :evidence :corpus :note "65 on a code-graph tool, that repo's every-failure-is-a-value style; baseline them")
   :mutable-escape        (assoc (:mutable-escape host/rules) :evidence :corpus)
   :js-prop-on-own-object (assoc (:js-prop-on-own-object typeflow/rules) :evidence :corpus :note "found render.cljs:367 shipped; corpus flag/clear")
   :reflection-unwarned   (assoc (:reflection-unwarned typeflow/rules) :evidence :corpus)}
  (into {} (map (fn [{:keys [id category instruction evidence note]}] [id (cond-> {:category category :instruction instruction :evidence (or evidence :unjudged)} note (assoc :note note))])) data/rules)))

(def applicability
  #{:machine-applicable :maybe-incorrect :has-placeholders :unspecified})

(defn- findings-in
  [zloc file typeflow-findings]
  (let [maps (map-loop/findings file zloc)
        taken (into #{} (map (juxt :line :column)) maps)]
    (mapv (fn [f] (let [{:keys [category instruction]} (get rules (:rule f))]
                    (assoc f :category category :instruction (or (:instruction f) instruction))))
          (-> (vec (fold/findings file zloc))
              (into maps)
              (into (loop-fold/findings file zloc taken))
              (into (cond-case/findings file zloc))
              (into (cond-build/findings file zloc))
              (into (let-chain/findings file zloc))
              (into (host/findings file zloc))
              (into (data/findings file zloc))
              (into (filter #(contains? #{:js-prop-on-own-object :reflection-unwarned} (:rule %))
                            typeflow-findings))))))

(defn- findings*
  [text file]
  (let [zloc (try (z/of-node* (parser/parse-string-all text))
                  (catch #?(:clj Exception :cljs :default) _ nil))]
    (if zloc
      (findings-in zloc file (typeflow/findings text file))
      [])))

(defn findings
  ([text file] (findings text file nil))
  ([text file resolve-idx]
   (binding [fold/*resolve* (resolve/for-file resolve-idx file)]
     (findings* text file))))

(defn findings-at
  [zloc file resolve-idx typeflow-findings]
  (binding [fold/*resolve* (resolve/for-file resolve-idx file)]
    (findings-in zloc file typeflow-findings)))
