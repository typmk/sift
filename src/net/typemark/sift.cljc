(ns net.typemark.sift
  (:require [clojure.string :as str]
            [net.typemark.sift.analysis :as analysis]
            [net.typemark.sift.complexity :as complexity]
            [net.typemark.sift.data :as data]
            [net.typemark.sift.docs :as docs]
            [net.typemark.sift.highlight :as highlight]
            [net.typemark.sift.json :as json]
            [net.typemark.sift.metrics :as metrics]
            [net.typemark.sift.parse :as p]
            [net.typemark.sift.portable.prose :as prose]
            [net.typemark.sift.registry :as registry]
            [net.typemark.sift.resolve :as resolve]
            [net.typemark.sift.typeflow :as typeflow]
            [net.typemark.sift.zip :as sz]
))

(def ^:private measures-of-unit #{:cognitive :cyclomatic :max-nesting :params})

(defn- invalid [rule msg]
  (throw (ex-info (str (:id rule) ": " msg) {:rule (:id rule)})))

(defn- validate
  [{:keys [id extends level evidence applicability] :as rule}]
  (when-not (and (keyword? id) (namespace id)) (invalid rule "a rule id is a qualified keyword, :ruleset/name"))
  (when-not (registry/checks extends) (invalid rule (str "extends " (pr-str extends) ", not one of " (sort registry/checks))))
  (when-not (some #{level} registry/levels) (invalid rule (str "level " (pr-str level) ", not one of " registry/levels)))
  (when-not (registry/rungs evidence) (invalid rule (str "evidence " (pr-str evidence) ", not one of " (sort registry/rungs))))
  (when-not (registry/applicability applicability) (invalid rule (str "applicability " (pr-str applicability))))
  (case extends
    (:existence :substitution) (when-not (or (contains? rule :match) (vector? (:either rule)) (string? (:head-ns rule)))
                                 (invalid rule "needs :match, :either or :head-ns"))
    :metric (when-not (and (measures-of-unit (:measure rule)) (number? (:max rule)))
              (invalid rule (str "needs :measure, one of " (sort measures-of-unit) ", and a numeric :max")))
    :script (when-not (fn? (:run rule)) (invalid rule "needs a :run function")))
  (when (and (= :substitution extends) (not (contains? rule :emit))) (invalid rule "needs :emit"))
  (doseq [k (:required rule)]
    (when (nil? (get rule k)) (invalid rule (str "needs " k))))
  rule)

(defn- resolve-rules
  [{:keys [rulesets rules]}]
  (let [chosen (set (or rulesets registry/default-rulesets))
        base (into {} (map (juxt :id identity)) registry/built-in)
        merged (reduce-kv (fn [m id v]
                            (let [v (if (keyword? v) {:level v} v)]
                              (cond
                                (contains? base id) (update m id merge v)
                                (:extends v) (assoc m id (assoc v :id id))
                                :else (throw (ex-info (str id ": no such rule, and no :extends to define one") {:rule id})))))
                          base
                          (or rules {}))]
    (->> (vals merged)
         (keep (fn [r]
                 (let [set' (keyword (namespace (:id r)))
                       r (merge {:evidence :unjudged :applicability :unspecified
                                 :level (get registry/rulesets set' :warning)}
                                r)]
                   (when (and (or (chosen set') (contains? rules (:id r)))
                              (not= :off (:level r)))
                     (validate r)))))
         (sort-by (comp str :id))
         vec)))

(defn- rank [level] (count (take-while #(not= level %) registry/levels)))

(defn- level-of [config k default]
  (let [l (get config k default)]
    (when-not (and (some #{l} registry/levels) (not= :off l))
      (throw (ex-info (str k " " (pr-str l) ", not one of " (vec (rest registry/levels))) {k l})))
    l))

(defn linter
  [{:keys [kondo oracle] :as config}]
  (let [a (when kondo (get (json/read-str kondo) "analysis"))]
    {:rules (resolve-rules config)
     :min-level (level-of config :min-level :info)
     :fail-level (level-of config :fail-level :error)
     :kondo a
     :resolution (when a (resolve/index a))
     :oracle oracle}))

(defn rules
  [linter]
  (mapv #(dissoc % :run) (:rules linter)))

(defn- test-path? [path]
  (boolean (re-find #"(^|/)test/|_test\.clj[sc]?$" (str path))))

(defn- suffix? [a b]
  (or (str/ends-with? (str a) (str b)) (str/ends-with? (str b) (str a))))

(defn- source
  [linter {:keys [path text test?]}]
  (let [{:keys [ok? root nodes error]} (p/parse-root text)
        zloc (when ok? (sz/of-root root))
        locs (delay (sz/locations zloc))
        ns-env (delay (binding [sz/*locations* @locs] (typeflow/ns-env zloc)))
        docs (delay (binding [sz/*locations* @locs] (docs/extract zloc)))
        host (typeflow/host-of path)
        {:keys [tags loaded externs]} (:oracle linter)
        resolution (resolve/for-file (:resolution linter) path)]
    {:path path :text text :host host :ok? ok? :error error
     :test? (if (some? test?) test? (test-path? path))
     :nodes nodes
     :zloc zloc
     :locs locs
     :ns-env ns-env
     :resolution resolution
     :classes tags
     :loaded loaded
     :loaded? (fn [p] (some #(suffix? p %) loaded))
     :typeflow (delay (binding [sz/*locations* @locs]
                        (typeflow/analysis zloc path {:var-tags (:vars tags) :classes tags :externs externs
                                                      :resolution (:resolution linter) :ns-env @ns-env})))
     :docs docs
     :prose (delay (prose/findings @docs))
     :units (delay (let [r (complexity/report-of root path)] (if (:ok? r) (:functions r) [])))}))

(defn- missing [linter rule]
  (some (fn [need] (when (nil? (get linter need)) need)) (sort (:needs rule))))

(defn- finding
  [path rule hit]
  (cond-> (merge {:message (:message rule) :applicability (:applicability rule)}
                 (dissoc hit :rule)
                 {:rule (:id rule)
                  :level (:level rule)
                  :file (or (:file hit) path)
                  :evidence (or (:evidence hit) (:evidence rule))})
    (:instruction rule) (assoc :instruction (:instruction rule))))

(defn- metric
  [file {:keys [measure max]}]
  (for [u (complexity/flatten-units @(:units file))
        :let [v (get u measure)]
        :when (and v (> v max))]
    {:line (:line u) :column 1 :end-line (:line u) :end-column 2
     :symbol (some-> (:name u) symbol)
     :message (str (:name u) " has " (name measure) " " v " (max " max "); cognitive " (:cognitive u)
                   ", cyclomatic " (:cyclomatic u) ", nesting " (:max-nesting u))
     :cognitive (:cognitive u) :cyclomatic (:cyclomatic u)
     :max-nesting (:max-nesting u) :params (:params u)}))

(defn- pattern-env [file]
  {:tags (when (= :jvm (:host file)) (:tags @(:typeflow file)))
   :classes (:classes file)
   :imports (:imports @(:ns-env file))})

(defn- file-findings
  [linter file]
  (let [active (filter (fn [r] (and (not (missing linter r))
                                    (not (:corpus r))
                                    (or (not= :test (:files r)) (:test? file))))
                       (:rules linter))
        pattern? #(contains? #{:existence :substitution} (:extends %))
        by-id (into {} (map (juxt :id identity)) active)
        patterns (filter pattern? active)]
    (binding [sz/*locations* @(:locs file)]
     (doall
      (concat
     (when (seq patterns)
       (for [h (data/findings (pattern-env file) patterns (:zloc file))]
         (finding (:path file) (by-id (:rule h)) h)))
     (for [r (remove pattern? active)
           h (case (:extends r)
               :metric (metric file r)
               :script ((:run r) file r))]
       (finding (:path file) r h)))))))

(defn- order [findings]
  (sort-by (juxt (comp str :file) #(or (:line %) 0) #(or (:column %) 0) (comp str :rule)) findings))

(defn lint
  [linter inputs]
  (let [files (mapv #(source linter %) inputs)
        {readable true unreadable false} (group-by (comp boolean :ok?) files)
        corpus (for [r (:rules linter)
                     :when (and (:corpus r) (not (missing linter r)))
                     h ((:run r) linter readable r)]
                 (finding nil r h))]
    {:findings (->> (concat (mapcat #(file-findings linter %) readable) corpus)
                    (filter #(>= (rank (:level %)) (rank (:min-level linter))))
                    order
                    vec)
     :errors (mapv (fn [f] {:file (:path f) :message (or (:error f) "unreadable")}) unreadable)
     :skipped (vec (for [r (:rules linter) :let [m (missing linter r)] :when m]
                     {:rule (:id r) :missing m}))}))

(defn failed?
  [linter {:keys [findings errors]}]
  (boolean (or (seq errors)
               (some #(>= (rank (:level %)) (rank (:fail-level linter))) findings))))

(defn prose-in
  [text]
  (let [{:keys [ok? root nodes]} (p/parse-root text)
        zloc (when ok? (sz/of-root root))]
    (->> (concat
          (for [d (docs/extract zloc) :when (string? (:text d))]
            (assoc (select-keys d [:line :column :end-line :end-column :text]) :kind :docstring))
          (for [n nodes
                :when (and (= :comment (:type n)) (not (:commented? n))
                           (not (and (= 1 (:line n)) (str/starts-with? (:text n) "#!"))))]
            {:kind :comment :line (:line n) :column (:col n) :end-line (:line n)
             :end-column (+ (:col n) (count (str/trimr (:text n))))
             :text (str/replace (str/trimr (:text n)) #"^;+ ?" "")}))
         (sort-by (juxt :line :column))
         vec)))

(defn units
  [text path]
  (let [r (complexity/report text path)] (if (:ok? r) (:functions r) [])))

(defn inferred
  [linter {:keys [path text]}]
  (let [{:keys [tags externs]} (:oracle linter)]
    (typeflow/inferred text path {:var-tags (:vars tags) :classes tags :externs externs
                                  :resolution (resolve/for-file (:resolution linter) path)})))

(defn parse-source [text] (p/parse text))

(defn leaves [nodes] (p/leaves nodes))

(defn cpd-image [text] (p/cpd-image text))

(defn spans [tokens] (highlight/spans tokens))

(defn measures [text] (metrics/measures text))

(defn line-data [nodes truth] (metrics/line-data nodes truth))

(defn symbols [kondo] (analysis/symbols kondo))
