(ns net.typemark.sift.shape
  "Places used as folds, and loops that are maps — the imperative SHAPE that
  no branch count sees. An atom accumulated over a `doseq` scores 0 cognitive
  and is still the thing an LLM writes when it has stopped thinking in values.

  Two rules, ported whole from an earlier `places` rule set, which had them
  as a standalone CLI with a corpus-as-spec harness. The corpus came with
  them: `test/…/corpus/clear` must stay silent and `corpus/flag` must fire,
  on the JVM and under bun. They are zipper rules over source TEXT, the
  third shape of rule this library carries beside the node-stream rules and
  the clj-kondo-analysis rules, and the same shape as `complexity`.

  Each finding carries `:instruction` — the rewrite an agent is told to
  apply — and, when the body is a lone `swap!`/`reset!` on the atom, a
  `:counterpart` form with `:applicability :machine-applicable` — clippy's four
  rungs, see `applicability`. sift never writes;
  the `--apply` that `places` had belongs to a gated editor, not a linter.

  `^:places/allow` on the binding peels a finding, as it did."
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
  "Every shape rule, with the category a consumer gates on (Credo's five:
  :refactor :readability :design :warning :consistency) and the instruction
  an agent is handed. A registry, not a set, so this is where a rule's
  metadata lives and a rule file is only its matcher and counterpart."
  (merge
  ;; :evidence is the rung the rule stands on — :compiler, :parity, :corpus,
  ;; :read, :unjudged — and :note the measurement. It lives HERE, on the
  ;; entry, so a rule cannot exist without saying what it rests on; the
  ;; suite fails on an entry without one.
  {fold/rule      {:category :refactor :instruction fold/instruction :evidence :corpus :note "17/17 with the standalone CLI on a code-graph tool"}
   map-loop/rule  {:category :refactor :instruction map-loop/instruction :evidence :corpus}
   loop-fold/rule {:category :refactor :instruction loop-fold/instruction :evidence :corpus :note "0 on hand-written code, 1 in clojure.core — fires on generated code"}
   cond-case/rule {:category :refactor :instruction cond-case/instruction :evidence :corpus :note "1 on a code-graph tool, 7 in clojure.core, all read"}
   cond-build/rule {:category :refactor :instruction cond-build/instruction :evidence :corpus
                    :note "8 of 12,248 let forms over 1,131 files; clojure.core's own defn rebuilds fdecl four times"}
   let-chain/rule  {:category :refactor :instruction let-chain/instruction :evidence :corpus
                    :note "13 of 12,248 let forms; 18 before refusing a conditional step and a qualified let — p/let is promise sequencing, not a chain"}
   ;; the host boundary — see host.cljc
   :catch-all-swallow     (assoc (:catch-all-swallow host/rules) :evidence :corpus :note "65 on a code-graph tool, that repo's every-failure-is-a-value style; baseline them")
   :mutable-escape        (assoc (:mutable-escape host/rules) :evidence :corpus)
   :js-prop-on-own-object (assoc (:js-prop-on-own-object typeflow/rules) :evidence :corpus :note "found render.cljs:367 shipped; corpus flag/clear")
   :reflection-unwarned   (assoc (:reflection-unwarned typeflow/rules) :evidence :corpus)}
  ;; rules.edn — each carries its own category, instruction and evidence
  (into {} (map (fn [{:keys [id category instruction evidence note]}] [id (cond-> {:category category :instruction instruction :evidence (or evidence :unjudged)} note (assoc :note note))])) data/rules)))

(def applicability
  "clippy's four rungs, so an editor knows what it may apply unasked:
  :machine-applicable — apply it; :maybe-incorrect — a form that is usually
  right; :has-placeholders — a form with a hole a human fills;
  :unspecified — a finding with no form."
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
              ;; the two host rules that live in typeflow's env
              (into (filter #(contains? #{:js-prop-on-own-object :reflection-unwarned} (:rule %))
                            typeflow-findings))))))

(defn- findings*
  [text file]
  ;; `of-node*` (edn* before rewrite-clj 1.1.45), not `of-string`:
  ;; `of-string` moves to the FIRST form, and a
  ;; walk from there sees only the ns form. Measured: every corpus file
  ;; read as clean.
  (let [zloc (try (z/of-node* (parser/parse-string-all text))
                  (catch #?(:clj Exception :cljs :default) _ nil))]
    (if zloc
      (findings-in zloc file (typeflow/findings text file))
      [])))

(defn findings
  "Source text -> findings, or [] if it does not read as forms. `file` is
  carried onto every finding and, when `resolve-idx` (from `resolve/index`)
  is given, picks this file's kondo resolution so aliased core vars are seen
  and shadowed ones are not."
  ([text file] (findings text file nil))
  ([text file resolve-idx]
   (binding [fold/*resolve* (resolve/for-file resolve-idx file)]
     (findings* text file))))

(defn findings-at
  "`findings` over a root zipper the caller already made, with the typeflow
  findings the two host rules are taken from."
  [zloc file resolve-idx typeflow-findings]
  (binding [fold/*resolve* (resolve/for-file resolve-idx file)]
    (findings-in zloc file typeflow-findings)))
