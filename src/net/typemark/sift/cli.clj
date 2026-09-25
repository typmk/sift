(ns net.typemark.sift.cli
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [net.typemark.sift :as sift]
            [net.typemark.sift.baseline :as baseline]
            [net.typemark.sift.json :as json]
            [net.typemark.sift.portable.prose :as prose]))

(def ^:private usage-text
  (str/join "\n"
            ["usage: sift lint [options] <path>..."
             "       sift rules [options]"
             "       sift docs-mirror <outdir> <path>..."
             "       sift vale-style <dir>"
             "       sift vale-sarif <vale.json>"
             "       sift oracle [--out DIR] <src-root>...   what the JVM compiler knows, run where the project loads"
             "       sift oracle --js [--out DIR] [closure-compiler.jar]   Closure's extern property names"
             ""
             "options:"
             "  --config F          default .sift.edn"
             "  --ruleset R         repeatable; replaces the config's rulesets"
             "  --rule ID=LEVEL     repeatable; LEVEL is off, info, warning or error"
             "  --min-level L       hide findings below L (default info)"
             "  --fail-level L      exit 1 at or above L (default error)"
             "  --kondo F           clj-kondo analysis JSON; otherwise clj-kondo is run if on PATH"
             "  --no-kondo          never run clj-kondo"
             "  --oracle DIR        what bin/oracle wrote for this project"
             "  --edn               findings as EDN"
             "  --baseline F        report only findings not in F"
             "  --write-baseline F  write today's findings to F"]))

(defn- fail! [code msg]
  (binding [*out* *err*] (println msg))
  (System/exit code))

(defn- usage [] (fail! 2 usage-text))

(def ^:private levels #{"off" "info" "warning" "error"})

(defn- options
  [args]
  (loop [[a & more] args opts {:paths [] :rulesets [] :rules {}}]
    (let [value (fn [] (or (first more) (usage)))]
      (cond
        (nil? a) opts
        (= a "--edn") (recur more (assoc opts :edn? true))
        (= a "--no-kondo") (recur more (assoc opts :no-kondo? true))
        (#{"--config" "--kondo" "--oracle" "--baseline" "--write-baseline"} a)
        (recur (rest more) (assoc opts (keyword (subs a 2)) (value)))
        (#{"--min-level" "--fail-level"} a)
        (let [v (value)] (if (levels v) (recur (rest more) (assoc opts (keyword (subs a 2)) (keyword v))) (usage)))
        (= a "--ruleset") (recur (rest more) (update opts :rulesets conj (keyword (value))))
        (= a "--rule") (if-let [[_ id lvl] (re-matches #"(.+)=(off|info|warning|error)" (value))]
                         (recur (rest more) (assoc-in opts [:rules (keyword id)] (keyword lvl)))
                         (usage))
        (str/starts-with? a "--") (usage)
        :else (recur more (update opts :paths conj a))))))

(defn- clj-file? [f] (re-find #"\.clj[csx]?$" (.getName (io/file f))))

(defn- expand
  [path]
  (let [f (io/file path)]
    (cond (.isFile f) (if (clj-file? f) [f] [])
          (.isDirectory f) (sort-by str (filter #(and (.isFile %) (clj-file? %)) (file-seq f)))
          :else (fail! 2 (str "sift: not a file or directory: " path)))))

(defn- relative [f]
  (let [cwd (.toPath (.getCanonicalFile (io/file ".")))]
    (str (.relativize cwd (.toPath (.getCanonicalFile (io/file f)))))))

(defn- read-edn [f] (when (and f (.exists (io/file f))) (edn/read-string (slurp f))))

(defn- run-kondo
  [paths]
  (try
    (let [{:keys [out]} (apply sh/sh "clj-kondo" "--lint"
                               (concat paths ["--config" "{:analysis {:arglists true :locals true} :output {:format :json}}"]))]
      (when (str/starts-with? (str/triml out) "{") out))
    (catch java.io.IOException _ nil)))

(defn- linter-of
  [{:keys [config kondo no-kondo? oracle rulesets rules paths min-level fail-level]}]
  (let [cfg (or (read-edn (or config ".sift.edn")) {})
        kondo-file (or kondo (when oracle (let [f (str oracle "/analysis.json")] (when (.exists (io/file f)) f))))
        kondo-text (cond kondo-file (slurp kondo-file)
                         (and (not no-kondo?) (seq paths)) (run-kondo paths))]
    (try
      (sift/linter (cond-> (update cfg :rules merge rules)
                     (seq rulesets) (assoc :rulesets (set rulesets))
                     min-level (assoc :min-level min-level)
                     fail-level (assoc :fail-level fail-level)
                     kondo-text (assoc :kondo kondo-text)
                     oracle (assoc :oracle {:tags (read-edn (str oracle "/tags.edn"))
                                            :loaded (some-> (read-edn (str oracle "/loaded.edn")) set)
                                            :externs (read-edn (str oracle "/externs.edn"))})))
      (catch clojure.lang.ExceptionInfo e
        (fail! 2 (str "sift: " (ex-message e)))))))

(defn- print-finding [{:keys [file line column level rule message instruction fix applicability evidence]}]
  (println (format "%s:%s:%s %s %s [%s] %s" file line (or column 1) (name level) (subs (str rule) 1) (name evidence) message))
  (when instruction (println (str "  instruction: " instruction)))
  (when fix (println (str "  fix (" (name applicability) "):\n    " (pr-str fix)))))

(def ^:private flag-for {:kondo "--kondo (or clj-kondo on PATH)" :oracle "--oracle"})

(defn- print-skipped [skipped]
  (binding [*out* *err*]
    (doseq [[need rs] (sort-by key (group-by :missing skipped))]
      (println (format "skipped %d rules, need %s: %s" (count rs) (flag-for need (name need))
                       (str/join " " (map #(subs (str (:rule %)) 1) rs)))))))

(defn- lint [args]
  (let [{:keys [paths edn? baseline write-baseline] :as opts} (options args)
        _ (when (empty? paths) (usage))
        linter (linter-of opts)
        files (mapcat expand paths)
        result (sift/lint linter (for [f files] {:path (relative f) :text (slurp f)}))
        findings (cond->> (:findings result)
                   baseline (baseline/new-findings (baseline/parse (slurp baseline))))
        result (assoc result :findings findings)]
    (when write-baseline
      (spit write-baseline (baseline/render (baseline/counts findings)))
      (println (count findings) "findings written to" write-baseline)
      (System/exit 0))
    (binding [*out* *err*]
      (doseq [{:keys [file message]} (:errors result)] (println (str file ": unreadable: " message))))
    (print-skipped (:skipped result))
    (cond edn? (prn findings)
          (seq findings) (run! print-finding findings)
          :else (println "no findings"))
    (flush)
    (System/exit (if (sift/failed? linter result) 1 0))))

(defn- rules [args]
  (doseq [{:keys [id level extends evidence needs]} (sift/rules (linter-of (assoc (options args) :no-kondo? true)))]
    (println (format "%-44s %-8s %-13s %-9s %s" (subs (str id) 1) (name level) (name extends) (name evidence)
                     (str/join " " (map name (sort needs)))))))

(defn- docs-mirror
  [outdir paths]
  (let [under (fn [root f]
                (let [rel (relative f)
                      rel (if (str/starts-with? rel "..")
                            (str (.relativize (.toPath (.getParentFile (.getCanonicalFile (io/file root))))
                                              (.toPath (.getCanonicalFile (io/file f)))))
                            rel)]
                  (when (some #{".."} (str/split rel #"/")) (fail! 2 (str "sift: refusing to mirror outside " outdir ": " rel)))
                  rel))
        files (for [root paths f (expand root)] [root f])
        written (for [[root f] files
                      :let [pieces (sift/prose-in (slurp f))]
                      :when (seq pieces)]
                  (let [lines (reduce (fn [acc {:keys [line text]}]
                                        (reduce (fn [acc [i l]]
                                                  (let [at (+ line i)]
                                                    (-> (into acc (repeat (max 0 (- at (count acc))) ""))
                                                        (update (dec at) #(if (str/blank? %) (str/triml l) (str % " " (str/triml l)))))))
                                                acc
                                                (map-indexed vector (str/split-lines text))))
                                      [] pieces)
                        out (io/file outdir (str (under root f) ".md"))]
                    (io/make-parents out)
                    (spit out (str (str/join "\n" lines) "\n"))
                    out))]
    (println (count (doall written)) "files mirrored to" outdir)))

(defn- vale-sarif
  [vale-json-file]
  (let [v (json/read-str (slurp vale-json-file))
        level (fn [s] (case s "error" "error" "warning" "warning" "note"))
        checks (distinct (for [[_ alerts] v a alerts] (get a "Check")))]
    {:version "2.1.0"
     :$schema "https://json.schemastore.org/sarif-2.1.0.json"
     :runs [{:tool {:driver {:name "vale"
                             :rules (vec (for [c checks] {:id c :defaultConfiguration {:level "warning"}}))}}
             :results (vec (for [[file alerts] v a alerts]
                             {:ruleId (get a "Check")
                              :level (level (get a "Severity"))
                              :message {:text (get a "Message")}
                              :locations [{:physicalLocation {:artifactLocation {:uri (str/replace file #"\.md$" "")}
                                                              :region {:startLine (get a "Line")}}}]}))}]}))

(defn- vale-style
  [dir]
  (let [yml (fn [level msg tokens]
              (str "extends: existence\nmessage: \"" msg "\"\nlevel: " level "\nignorecase: true\ntokens:\n"
                   (str/join (map #(str "  - " % "\n") tokens))))]
    (.mkdirs (io/file dir))
    (spit (io/file dir "Hedge.yml") (yml "warning" "Hedge in docstring: '%s'. A docstring is the contract." prose/hedges))
    (spit (io/file dir "Placeholder.yml") (yml "error" "Docstring is a placeholder: '%s'." prose/placeholder-tokens))
    (println "wrote Hedge.yml and Placeholder.yml to" dir)))

(defn- repo-root
  []
  (-> (io/resource "net/typemark/sift/cli.clj") io/file .getParentFile .getParentFile .getParentFile .getParentFile .getParentFile str))

(defn- externs!
  [out jar]
  (let [jar (or jar
                (->> (file-seq (io/file (System/getProperty "user.home") ".m2/repository/com/google/javascript/closure-compiler"))
                     (map str)
                     (filter #(re-find #"closure-compiler-v[^/]*\.jar$" %))
                     (remove #(str/includes? % "sources"))
                     sort last))
        _ (when-not jar (fail! 2 "no closure-compiler jar found; pass one"))
        inner (str (java.nio.file.Files/createTempDirectory "sift" (make-array java.nio.file.attribute.FileAttribute 0)) "/externs.zip")]
    (with-open [zip (java.util.zip.ZipFile. (str jar))
                in (.getInputStream zip (.getEntry zip "externs.zip"))
                o (io/output-stream inner)]
      (io/copy in o))
    (with-open [z (java.util.zip.ZipFile. inner)]
      (let [names (into (sorted-set)
                        (for [e (enumeration-seq (.entries z)) :when (str/ends-with? (.getName e) ".js")
                              [_ p] (re-seq #"\.([A-Za-z_$][A-Za-z0-9_$]*)" (slurp (.getInputStream z e)))]
                          p))]
        (.mkdirs (io/file out))
        (spit (io/file out "externs.edn") (pr-str names))
        (binding [*out* *err*] (println (count names) "property names from" (.getName (io/file jar)) "->" (str out "/externs.edn")))))))

(defn- oracle!
  [args]
  (let [args (vec args)]
    (if (some #{"--js"} args)
      (let [out (or (some->> args (drop-while #(not= "--out" %)) second) "oracle")
            jar (first (remove (into #{"--js" "--out"} [out]) args))]
        (externs! out jar))
      (let [cmd (into ["clojure" "-Sdeps" (pr-str {:deps {'net.typemark/sift-oracle {:local/root (str (repo-root) "/oracle")}}})
                       "-M" "-m" "net.typemark.sift.oracle"]
                      args)
            p (-> (ProcessBuilder. ^java.util.List cmd) .inheritIO .start)]
        (System/exit (.waitFor p))))))

(defn -main
  [& [cmd & args]]
  (case cmd
    "lint" (lint args)
    "rules" (rules args)
    "docs-mirror" (let [[out & paths] args] (if (and out (seq paths)) (docs-mirror out paths) (usage)))
    "vale-style" (if-let [d (first args)] (vale-style d) (usage))
    "vale-sarif" (if-let [f (first args)] (println (json/write-str (vale-sarif f))) (usage))
    "oracle" (oracle! args)
    (usage))
  (flush)
  (System/exit 0))
