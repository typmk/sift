(ns com.typemark.sift
  "The analysis surface, named in one place.

  Everything under this namespace is Clojure analysis with no SonarQube in it:
  a rewrite-clj node stream, rules over that stream, rules over clj-kondo's
  analysis output, and the measures derived from either. Nothing here imports
  org.sonar.

  It exists because source-sensor reached into sixteen namespaces directly.
  That is a shared codebase, not a dependency -- the analysis could not be
  consumed by anything else without dragging the same sixteen requires along,
  and could not be extracted without breaking every one of them. One seam makes
  both possible: a consumer requires `scan`, and the rules behind it can move,
  split or gain siblings without any consumer noticing.

  The rules were already uniform and it was not written down. Six take a node
  stream and are named `findings`; two take clj-kondo's analysis text. That is
  the whole taxonomy, and it is the reason this file is short.

  NOT tools.analyzer, for the reason parse.clj gives: resolving and
  macroexpanding means loading the namespace, and a scanner that evaluates the
  code it measures runs your side effects on the CI box. A REPL-side consumer
  that has already loaded the code is a different context and may rank higher;
  this surface is for the one that has not."
  (:require #?(:clj  [com.typemark.sift.data-rules :refer [load-edn]]
               :cljs [com.typemark.sift.data-rules :refer-macros [load-edn]]) [clojure.string :as str]
            [com.typemark.sift.access :as access]
            [com.typemark.sift.data :as data]
            [com.typemark.sift.analysis :as analysis]
            [com.typemark.sift.callgraph :as callgraph]
            [com.typemark.sift.complexity :as complexity]
            [com.typemark.sift.concurrency :as concurrency]
            [com.typemark.sift.dictionary :as dictionary]
            [com.typemark.sift.highlight :as highlight]
            [com.typemark.sift.interop :as interop]
            [com.typemark.sift.metrics :as metrics]
            [com.typemark.sift.parse :as p]
            [com.typemark.sift.prose :as prose]
            [com.typemark.sift.regex :as regex]
            [com.typemark.sift.resolve :as resolve]
            [com.typemark.sift.security :as security]
            [com.typemark.sift.shape :as shape]
            [com.typemark.sift.tests :as tests]
            [com.typemark.sift.typeflow :as typeflow]
            [com.typemark.sift.web :as web]))

;; ── structure ──────────────────────────────────────────────────────────────

(defn parse-source
  "Source text -> {:ok? :nodes :error}.

  NOT `parse`. A var named `parse` on this namespace and the child namespace
  com.typemark.sift.parse compile to the SAME JavaScript path, so in
  ClojureScript one silently overwrites the other and every call becomes
  \"parse is not a function\" at runtime. On the JVM they coexist, which is why
  this survived a green suite and a clean compile and only appeared when the
  code was actually run under bun. No var here may share a name with a child
  namespace segment."
  [text]
  (p/parse text))
(defn leaves   "Token nodes, for CPD and highlighting."  [nodes] (p/leaves nodes))
(defn cpd-image "Duplication image for one token."       [text]  (p/cpd-image text))
(defn spans
  "Highlight spans over token nodes. The token CLASS is analysis; mapping it to
  a renderer's palette is the consumer's job, which is why this returns spans
  rather than anything Sonar-shaped."
  [tokens] (highlight/spans tokens))

;; ── measures ───────────────────────────────────────────────────────────────

(defn measures
  "Size and complexity. Given source TEXT, complexity and cognitive come from
  the unit engine (complexity.cljc) summed over the file; given a node
  stream — the shape sonar-clojure's sensor still passes — the node count,
  which is the older, coarser score."
  [text-or-nodes]
  (if (string? text-or-nodes)
    (metrics/measures text-or-nodes)
    (metrics/from-nodes text-or-nodes)))
(defn line-data "Per-line measures."             [nodes truth]   (metrics/line-data nodes truth))
(defn unit-complexity
  "Per-UNIT cognitive and cyclomatic complexity, max nesting and parameter
  count, over source TEXT rather than the node stream — the walk needs the
  tree. NOT `complexity`: that is the child namespace's name, and the
  `parse` rule above applies — measured, `sift/complexity` compiled and left
  `com.typemark.sift.complexity.report` undefined under bun. `path` picks the `#?` branch (.cljs -> :cljs, else :clj).
  {:ok? true :functions [unit …]} or {:ok? false :error msg}."
  ([text] (complexity/report text nil))
  ([text path] (complexity/report text path)))
(defn resolution
  "clj-kondo analysis JSON -> the per-file resolution index `shape-findings`
  and `analyze` take. Build once per analysis, not per file."
  [analysis-text] (resolve/index analysis-text))
(defn var-tags
  "{\"ns/name\" tag} over a corpus's source TEXTS — every defn whose name or
  first arglist carries a ^Tag — merged with `extra` (bin/var-tags' dump for
  libraries). Build once; `analyze` uses it, through `:resolution`, to tag a
  call's result. Measured on lume: reflection precision 0.68 -> 0.81."
  ([texts] (var-tags texts nil))
  ([texts extra] (merge extra (apply merge (map typeflow/var-tags texts)))))
(defn shape-findings
  "Places used as folds and loops that are maps, over source TEXT — the
  `places` rules (agentia, DEFNET-4), now here. Each finding carries
  `:instruction` and, when mechanical, a `:counterpart` form. With a
  `resolution`, aliased core vars are seen and shadowed ones are not."
  ([text path] (shape/findings text path))
  ([text path resolution] (shape/findings text path resolution)))
(defn complexity-findings
  "Units whose cognitive complexity exceeds `threshold` (default 15, Sonar's),
  in the same finding shape as `findings`, under rule :cognitive-complexity."
  ([text path] (complexity/findings text path))
  ([text path threshold] (complexity/findings text path threshold)))
(defn symbols   "Symbol table, from clj-kondo analysis." [text]   (analysis/symbols text))
(defn tenanted? "Does the corpus name a tenant anywhere? see access/tenanted?" [texts] (access/tenanted? texts))

;; ── rules ──────────────────────────────────────────────────────────────────

(def node-rules
  "Rules over the node stream, each with the Credo category its findings
  carry. Order fixed so output is stable across runs. `:tests` is separate
  rather than listed here because it applies only to test sources, and a
  rule that silently returns nothing on main sources would be
  indistinguishable from one that is broken."
  [[:warning security/findings]
   [:warning interop/all-findings]
   [:warning concurrency/findings]
   [:warning regex/findings]
   [:warning web/findings]
   [:design  access/findings]])

(defn seeds
  "Taint sources and sinks this file contributes to the interprocedural pass."
  [nodes]
  (security/seeds nodes))

(defn findings
  "Every node-stream finding for one file. `:test?` adds the test-only rules."
  [nodes & {:keys [test?]}]
  (concat (mapcat (fn [[_ f]] (f nodes)) node-rules)
          (when test? (tests/findings nodes))))

;; ── one entry point ────────────────────────────────────────────────────────

(def node-rules-registry
  "The node-stream rules — strings in their namespaces, so their category,
  rung and measurement live here. :read means every hit on lume, sift and
  agentia was read at its line on 2026-08-27; :corpus means a flag/clear pair
  in node_rules_test, security_test, interop_test or dictionary_test. A pair
  proves a rule fires and stays silent; it does not say how it does on real
  code, and 0 hits on seven corpora is not evidence either way."
  {:permissive-file-permissions {:evidence :read :note "4/4 wrong (HTTP status keys); fixed — a call argument under a mode-setting head"}
   :hardcoded-credential {:evidence :read :note "4/4 wrong (*-secret-type slugs); fixed — a name ending -type/-kind names a kind"}
   :unscoped-tenant-query {:evidence :read :note "43 on lume unjudgeable without the data model; 5/5 wrong on agentia, now vacuous where the corpus names no tenant"}
   :ambiguous-owner-check {:evidence :read :note "8 on lume, unjudged: needs the data model"}
   :csrf-protection-absent {:evidence :read :note "2 on lume, token-authenticated routes; the :delete-as-argument hits fixed"}
   :side-effect-in-swap {:evidence :read :note "1 on lume, the CAS-with-a-decision idiom; fixed"}
   :xml-external-entity {:evidence :corpus :note "interop_test, hardened and not"}
   :operator-as-party {:evidence :corpus :note "node_rules_test; lume-specific by construction"}
   :discarded-future {:evidence :corpus :note "security_test"}
   :trust-all-certificates {:evidence :corpus :note "security_test"}
   :redos-vulnerable-regex {:evidence :corpus :note "node_rules_test; measured 0/11 true positives once, before the brace-nesting rule"}
   :partial-match-validation {:evidence :corpus :note "node_rules_test"}
   :cookie-missing-security-flags {:evidence :corpus :note "node_rules_test"}
   :sensitive-data-logged {:evidence :corpus :note "node_rules_test"}
   :xss-unescaped-output {:evidence :corpus :note "node_rules_test"}
   :empty-test {:evidence :corpus :note "node_rules_test"}
   :testing-without-assertion {:evidence :corpus :note "node_rules_test"}
   :test-with-no-effect {:evidence :corpus :note "node_rules_test"}
   :banned-term {:evidence :corpus :note "dictionary_test"}
   :unparseable {:evidence :corpus :note "not a rule: the file did not read"}})

(def registries
  "Every rule sift can emit, with its rung: the shape registry (shape, host
  and data rules), prose, typeflow's predictions, complexity, and the node
  rules above. `evidence` is what a reader of the README, the CLI or a
  finding gets; one place per rule."
  (delay (merge typeflow/rules prose/rules node-rules-registry shape/rules
                {:cognitive-complexity {:evidence :parity :note "5,519/5,531 units vs cccc on defnet; Spearman 0.989 vs SonarJS on LightTable"}})))

(defn evidence
  "{rule {:evidence rung :note …}} over every registry — the ledger, derived."
  []
  (into {} (map (fn [[k v]] [k (select-keys v [:evidence :note])])) @registries))

(defn evidence-of
  "The rung a rule stands on, from its registry entry; :unjudged when the
  entry has none — which is what an unlisted rule IS."
  [rule _family]
  (or (get-in @registries [rule :evidence]) :unjudged))

(defn- normalize
  "Every finding, whatever produced it, in one shape: a keyword :rule, a
  :family naming the producer, a :category, an :applicability, an
  :instruction, and an :evidence rung from evidence.edn. The node-stream
  rules predate the last four and carried a string rule; nothing
  downstream should have to know which family a finding came from to read
  it, or which README paragraph to trust it by."
  [family category f]
  (let [rule (if (keyword? (:rule f)) (:rule f) (keyword (:rule f)))
        fam (or (:family f) family)]
    (-> f
        (assoc :rule rule :family fam)
        (update :category #(or % category))
        (update :applicability #(or % :unspecified))
        (update :instruction #(or % (:message f)))
        (assoc :evidence (evidence-of rule fam)))))

(defn- prose-for
  "The prose findings for `path` out of a `prose-findings` map, whose keys
  are however clj-kondo was invoked — matched by the longest suffix."
  [prose path]
  (when (and prose path)
    (some->> (keys prose)
             (filter #(or (str/ends-with? path %) (str/ends-with? % path)))
             (sort-by count >)
             first
             (get prose))))

(defn analyze
  "Everything sift can say about one file, in one call and one shape.

    {:text       source
     :path       the file, for #? branch, the .clj/.cljs host rules and
                 prose lookup
     :test?      run the test-only rules too
     :resolution (sift/resolution kondo-json), optional
     :prose      (sift/prose-findings kondo-json), optional — built once
                 per analysis, this picks the entries for :path
     :tenanted?  (sift/tenanted? texts) over the corpus; default true.
                 false switches unscoped-tenant-query off as vacuous
     :var-tags   {ns/name tag} — bin/oracle's :vars plus sift/var-tags
     :classes    bin/oracle's :classes — the host's own method, constructor
                 and field table; typeflow judges overloads with it}

  -> {:ok? true
      :findings [f …]   every rule family, normalised — see `normalize`:
                        :node :shape :complexity :prose :typeflow
      :units    [u …]   per-unit complexity, nested units as :children
      :inferred [{:name :line :slot -1 :type} …]  return tags inferred for
                        unhinted defns (typeflow/inferred)
      :seeds    {…}     this file's taint sources and sinks, for
                        `interprocedural`}
  or {:ok? false :error msg} when the source does not read. A consumer that
  wants one family filters on :family — :node :shape :complexity :prose —
  rather than calling four functions."
  [{:keys [text path test? resolution prose var-tags classes tenanted?] :or {tenanted? true}}]
  (let [{:keys [ok? nodes error]} (p/parse text)]
    (if-not ok?
      {:ok? false :error (or error "unparseable")}
      (let [node   (concat (for [[cat f] node-rules
                                 ;; the one rule with a corpus-level switch — see access/tenanted?
                                 x (if (= f access/findings) (f nodes :tenanted? tenanted?) (f nodes))]
                             (normalize :node cat x))
                           (when test? (map #(normalize :node :design %) (tests/findings nodes))))
            ;; the data engine reads typeflow's tag environment and the oracle's
            ;; table, so a rule as data can ask what a receiver is
            shape  (binding [data/*tags* (when (= :jvm (typeflow/host-of path))
                                           (typeflow/tags text path {:var-tags var-tags :classes classes :resolution resolution}))
                             data/*classes* classes
                             data/*imports* (typeflow/imports text)]
                     (doall (map #(normalize :shape :refactor %) (shape/findings text path resolution))))
            cx     (complexity/report text path)
            over   (map #(normalize :complexity :refactor
                                    (assoc % :category :refactor
                                             :instruction "Split the unit: one branch per helper, or lift the nested lambda that carries the score."))
                        (complexity/findings text path))
            doc    (map #(normalize :prose :readability %) (prose-for prose path))
            flow   (->> (typeflow/findings text path {:var-tags var-tags :classes classes :resolution resolution})
                        ;; the two host rules already arrive via shape/findings
                        (remove #(contains? #{:js-prop-on-own-object :reflection-unwarned} (:rule %)))
                        (map #(normalize :typeflow :warning
                                         (assoc % :instruction "Hint the receiver or operands (^String s, ^long n), or cast (long x); the host compiler takes the slow path where the tag runs out.")))
                        ;; without bin/oracle's table the walker still runs, but what it
                        ;; says was not judged the way the :compiler rung means: say so
                        (map #(if (and (nil? classes) (= :jvm (typeflow/host-of path))) (assoc % :evidence :unjudged :note "no oracle; run sift/bin/oracle in the project") %)))]
        {:ok? true
         :findings (vec (concat node shape over doc flow))
         ;; return types the walker can name for unhinted defns — defnet's
         ;; :inferred rung, which had no producer
         :inferred (typeflow/inferred text path {:var-tags var-tags :classes classes :resolution resolution})
         :units (if (:ok? cx) (:functions cx) [])
         :seeds (security/seeds nodes)}))))

(defn prose-findings
  "Rules over docstrings, which need clj-kondo's analysis rather than the
  node stream: the banned-term dictionary, and the shapes generated
  docstrings have (restates the name, hedges, parameters unnamed,
  placeholder, namespace undocumented). {filename [finding …]}."
  [analysis-text]
  (merge-with into
              (dictionary/findings analysis-text)
              (prose/findings analysis-text)))

(defn interprocedural
  "Taint paths crossing function boundaries. Silent without seeds, by design:
  the direct findings are unaffected and a partial answer is not an error."
  [analysis-text {:keys [taints reaches] :as seeds}]
  (when-not (or (empty? taints) (empty? reaches))
    (callgraph/findings analysis-text seeds)))
