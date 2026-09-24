(ns net.typemark.sift
  (:require [clojure.string :as str]
            [net.typemark.sift.access :as access]
            [net.typemark.sift.data :as data]
            [net.typemark.sift.analysis :as analysis]
            [net.typemark.sift.callgraph :as callgraph]
            [net.typemark.sift.complexity :as complexity]
            [net.typemark.sift.concurrency :as concurrency]
            [net.typemark.sift.dictionary :as dictionary]
            [net.typemark.sift.highlight :as highlight]
            [net.typemark.sift.interop :as interop]
            [net.typemark.sift.metrics :as metrics]
            [net.typemark.sift.parse :as p]
            [net.typemark.sift.prose :as prose]
            [net.typemark.sift.regex :as regex]
            [net.typemark.sift.resolve :as resolve]
            [net.typemark.sift.security :as security]
            [net.typemark.sift.shape :as shape]
            [net.typemark.sift.tests :as tests]
            [net.typemark.sift.typeflow :as typeflow]
            [net.typemark.sift.web :as web]
            [net.typemark.sift.zip :as sz]))

(defn parse-source
  [text]
  (p/parse text))
(defn leaves   [nodes] (p/leaves nodes))
(defn cpd-image [text]  (p/cpd-image text))
(defn spans
  [tokens] (highlight/spans tokens))

(defn measures
  [text-or-nodes]
  (if (string? text-or-nodes)
    (metrics/measures text-or-nodes)
    (metrics/from-nodes text-or-nodes)))
(defn line-data [nodes truth]   (metrics/line-data nodes truth))
(defn unit-complexity
  ([text] (complexity/report text nil))
  ([text path] (complexity/report text path)))
(defn resolution
  [analysis-text] (resolve/index analysis-text))
(defn var-tags
  ([texts] (var-tags texts nil))
  ([texts extra] (merge extra (apply merge (map typeflow/var-tags texts)))))
(defn shape-findings
  ([text path] (shape/findings text path))
  ([text path resolution] (shape/findings text path resolution)))
(defn complexity-findings
  ([text path] (complexity/findings text path))
  ([text path threshold] (complexity/findings text path threshold)))
(defn symbols   [text]   (analysis/symbols text))
(defn tenanted? [texts] (access/tenanted? texts))

(def node-rules
  [security/findings interop/all-findings concurrency/findings
   regex/findings web/findings access/findings])

(defn seeds
  [nodes]
  (security/seeds nodes))

(defn findings
  [nodes & {:keys [test?]}]
  (concat (mapcat (fn [f] (f nodes)) node-rules)
          (when test? (tests/findings nodes))))

(def node-rules-registry
  {:permissive-file-permissions {:category :security :evidence :read :note "4/4 wrong (HTTP status keys); fixed — a call argument under a mode-setting head"}
   :hardcoded-credential {:category :security :evidence :read :note "4/4 wrong (*-secret-type slugs); fixed — a name ending -type/-kind names a kind"}
   :unscoped-tenant-query {:category :security :evidence :read :note "43 on a production service, unjudgeable without the data model; 5/5 wrong on a single-user library, now vacuous where the corpus names no tenant"}
   :ambiguous-owner-check {:category :security :evidence :read :note "8 on a production service, unjudged: needs the data model"}
   :csrf-protection-absent {:category :security :evidence :read :note "2 on a production service, token-authenticated routes; the :delete-as-argument hits fixed"}
   :side-effect-in-swap {:category :correctness :evidence :read :note "1 on a production service, the CAS-with-a-decision idiom; fixed"}
   :xml-external-entity {:category :security :evidence :corpus :note "interop_test, hardened and not"}
   :discarded-future {:category :correctness :evidence :corpus :note "security_test"}
   :trust-all-certificates {:category :security :evidence :corpus :note "security_test"}
   :redos-vulnerable-regex {:category :security :evidence :corpus :note "node_rules_test; measured 0/11 true positives once, before the brace-nesting rule"}
   :partial-match-validation {:category :security :evidence :corpus :note "node_rules_test"}
   :cookie-missing-security-flags {:category :security :evidence :corpus :note "node_rules_test"}
   :sensitive-data-logged {:category :security :evidence :corpus :note "node_rules_test"}
   :xss-unescaped-output {:category :security :evidence :corpus :note "node_rules_test"}
   :empty-test {:category :correctness :evidence :corpus :note "node_rules_test"}
   :testing-without-assertion {:category :correctness :evidence :corpus :note "node_rules_test"}
   :test-with-no-effect {:category :correctness :evidence :corpus :note "node_rules_test"}
   :banned-term {:category :refactor :evidence :corpus :note "dictionary_test"}
   :unparseable {:category :correctness :evidence :corpus :note "not a rule: the file did not read"}
   :interprocedural-taint {:category :security :evidence :unjudged :note "21 on a code-graph tool, every one a CLI path from argv/env toward fs or sh; not yet read against the sources it names"}})

(def registries
  (delay (merge typeflow/rules prose/rules node-rules-registry shape/rules
                {:cognitive-complexity {:category :complexity :evidence :parity :note "5,519/5,531 units vs cccc on a code-graph tool; Spearman 0.989 vs SonarJS on LightTable"}})))

(def categories
  #{:refactor :complexity :performance :security :correctness :documentation})

(defn evidence
  []
  (into {} (map (fn [[k v]] [k (select-keys v [:category :evidence :note])])) @registries))

(defn evidence-of
  [rule _family]
  (or (get-in @registries [rule :evidence]) :unjudged))

(defn- normalize
  [family category f]
  (let [rule (if (keyword? (:rule f)) (:rule f) (keyword (:rule f)))
        fam (or (:family f) family)]
    (-> f
        (assoc :rule rule :family fam)
        (assoc :category (or (get-in @registries [rule :category]) (:category f) category))
        (update :applicability #(or % :unspecified))
        (update :instruction #(or % (:message f)))
        (assoc :evidence (evidence-of rule fam)))))

(defn- prose-for
  [prose path]
  (when (and prose path)
    (some->> (keys prose)
             (filter #(or (str/ends-with? path %) (str/ends-with? % path)))
             (sort-by count >)
             first
             (get prose))))

(defn analyze
  [{:keys [text path test? resolution prose var-tags classes loaded tenanted? vocabulary] :or {tenanted? true}}]
  (let [{:keys [ok? root nodes error]} (p/parse-root text)]
    (if-not ok?
      {:ok? false :error (or error "unparseable")}
      (let [zloc   (sz/of-root root)
            locs   (sz/locations zloc)
            jvm?   (= :jvm (typeflow/host-of path))
            topts  {:var-tags var-tags :classes classes :resolution resolution
                    :ns-env (binding [sz/*locations* locs] (typeflow/ns-env zloc))
                    :annotate? jvm?}
            preds  (binding [sz/*locations* locs]
                     (typeflow/predictions-at zloc path (typeflow/host-of path) topts))
            tflow  (typeflow/findings-of preds)
            node   (concat (for [f node-rules
                                 x (if (= f access/findings)
                                     (f nodes :tenanted? tenanted? :shared-ns (:shared-ns vocabulary))
                                     (f nodes))]
                             (normalize :node nil x))
                           (when test? (map #(normalize :node :correctness %) (tests/findings nodes))))
            shape  (binding [sz/*locations* locs
                             data/*tags* (when jvm? (typeflow/tags-of preds))
                             data/*classes* classes
                             data/*imports* (:imports (:ns-env topts))]
                     (doall (map #(normalize :shape :refactor %) (shape/findings-at zloc path resolution tflow))))
            cx     (complexity/report-of root path)
            over   (map #(normalize :complexity :complexity
                                    (assoc % :category :complexity
                                             :instruction "Split the unit: one branch per helper, or lift the nested lambda that carries the score."))
                        (complexity/over-threshold cx complexity/default-threshold))
            doc    (map #(normalize :prose :documentation %) (prose-for prose path))
            flow   (->> tflow
                        (remove #(contains? #{:js-prop-on-own-object :reflection-unwarned} (:rule %)))
                        (map #(normalize :typeflow :performance
                                         (assoc % :instruction "Hint the receiver or operands (^String s, ^long n), or cast (long x); the host compiler takes the slow path where the tag runs out.")))
                        (map #(cond (and (nil? classes) jvm?)
                                    (assoc % :evidence :unjudged :note "no oracle; run `sift oracle` in the project")
                                    (and loaded (not (some (fn [l] (or (str/ends-with? (str path) l) (str/ends-with? l (str path)))) loaded)))
                                    (assoc % :evidence :unjudged :note "the oracle's compiler did not load this file")
                                    :else %)))]
        {:ok? true
         :findings (vec (concat node shape over doc flow))
         :inferred (typeflow/inferred text path {:var-tags var-tags :classes classes :resolution resolution})
         :units (if (:ok? cx) (:functions cx) [])
         :seeds (security/seeds nodes)}))))

(defn prose-findings
  ([analysis-text] (prose-findings analysis-text nil))
  ([analysis-text vocabulary]
   (merge-with into
               (dictionary/findings analysis-text (:banned vocabulary))
               (prose/findings analysis-text))))

(defn interprocedural
  [analysis-text {:keys [taints reaches] :as seeds}]
  (when-not (or (empty? taints) (empty? reaches))
    (callgraph/findings analysis-text seeds)))
