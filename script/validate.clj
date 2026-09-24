(ns validate
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]
            [clojure.set :as set]
            [clojure.string :as str]
            [net.typemark.sift :as sift]
            [net.typemark.sift.typeflow :as tf]
            [net.typemark.sift.resolve :as resolve]
            [net.typemark.sift.json :as json]))

(def here
  (fs/parent (fs/parent (fs/real-path *file*))))

(defn expand [s] (str/replace s #"^~" (System/getProperty "user.home")))
(defn tail2 [p] (str/join "/" (take-last 2 (str/split (str p) #"/"))))

(defn judge
  [root odir kind]
  (let [kind (keyword kind)
        js? (fs/exists? (str odir "/closure.log"))
        text (slurp (str odir (if js? "/closure.log" "/notes.edn")))
        code->kind {:assay.note/boxed-math :boxed-math :assay.note/reflection :reflection}
        assay-notes (for [l (str/split-lines text) :when (str/starts-with? l "#:assay.note")
                          :let [n (edn/read-string l) sp (:assay.note/span n)]]
                      {:file (str (:assay/file sp)) :line (:assay/line sp) :col (:assay/col sp) :kind (code->kind (:assay.note/code n))})
        closure-notes (for [[_ k file line col] (re-seq #"(?m)^------ WARNING #\d+ - (\S*) -+\s*\n File: (\S+?):(\d+)(?::(\d+))?" text)]
                        {:file file :line (parse-long line) :col (some-> col parse-long)
                         :kind ({":infer-warning" :uninferred ":undeclared-var" :undeclared} k :closure)})
        loaded (let [f (str odir "/loaded.edn")] (when (fs/exists? f) (set (edn/read-string (slurp f)))))
        suffix? (fn [a b] (or (str/ends-with? a b) (str/ends-with? b a)))
        canon (fn [f] (if loaded (or (some #(when (suffix? (str f) %) %) loaded) (tail2 f)) (tail2 f)))
        oracle (set (for [n (concat assay-notes closure-notes)
                          :when (= kind (:kind n))
                          :when (or (nil? loaded) (some #(suffix? (:file n) %) loaded))]
                      [(canon (:file n)) (:line n) (:col n)]))
        host (if js? :js :jvm)
        files (cond->> (map #(str (fs/absolutize %)) (fs/glob root "**.{clj,cljc,cljs}"))
                loaded (filter (fn [f] (some #(suffix? (str f) %) loaded))))
        analysis (let [f (str odir "/analysis.json")] (when (fs/exists? f) (resolve/index (get (json/read-str (slurp f)) "analysis"))))
        dump (let [f (str odir "/tags.edn")] (when (fs/exists? f) (edn/read-string (slurp f))))
        externs (let [f (str odir "/externs.edn")] (when (fs/exists? f) (edn/read-string (slurp f))))
        preds (set (for [f files
                         p (tf/predictions (slurp f) f host {:var-tags (:vars dump) :classes dump :externs externs :resolution analysis})
                         :when (= kind (:kind p))]
                     [(canon f) (:line p) (:column p)]))
        tp (count (set/intersection oracle preds))]
    {:oracle (count oracle) :predicted (count preds) :tp tp :fp (- (count preds) tp) :fn (- (count oracle) tp)
     :p (if (pos? (count preds)) (/ tp (double (count preds))) 0.0)
     :r (if (pos? (count oracle)) (/ tp (double (count oracle))) 0.0)
     :fns (sort (set/difference oracle preds)) :fps (sort (set/difference preds oracle))}))

(defn- newest
  [root]
  (->> (fs/glob root "**.{clj,cljc,cljs}")
       (map #(.toMillis (fs/last-modified-time %)))
       (reduce max 0)))

(defn- staleness
  [root oracle]
  (let [dump (first (filter fs/exists? [(str oracle "/notes.edn") (str oracle "/closure.log")]))
        dumped (when dump (.toMillis (fs/last-modified-time dump)))
        src (newest root)]
    (when (and dumped (pos? src) (> src dumped))
      (int (Math/ceil (/ (- src dumped) 86400000.0))))))

(defn -main
  [& args]
  (let [residue? (some #{"--residue"} args)
        names (disj (set args) "--residue")
        manifests (->> (fs/glob (str here "/corpora") "*.edn") (map str) sort
                       (map #(edn/read-string (slurp %)))
                       (filter #(or (empty? names) (names (:name %)))))]
    (println "typeflow vs the host compiler, per corpus")
    (printf "%-12s %-10s %-11s %s%n" "corpus" "role" "kind" "oracle predicted tp fp fn  precision recall")
    (doseq [{:keys [name root oracle role kinds]} manifests
            :let [root (expand root) oracle (expand oracle)]]
      (if-not (and (or (fs/exists? (str oracle "/notes.edn")) (fs/exists? (str oracle "/closure.log"))) (fs/exists? root))
        (printf "%-12s %-10s SKIPPED — %s%n" name (clojure.core/name role)
                (if (fs/exists? root) (str "no oracle at " oracle " (run sift oracle in the project)") (str "no source at " root)))
        (do
         (when-let [d (staleness root oracle)]
           (printf "%-12s %-10s STALE — the oracle is %d day(s) behind this source; re-run sift oracle before believing the score%n"
                   name (clojure.core/name role) d))
         (doseq [kind (or kinds (if (fs/exists? (str oracle "/closure.log")) ["uninferred"] ["boxed-math" "reflection"]))]
          (let [{:keys [oracle predicted tp fp fn p r fns fps]} (judge root oracle kind)]
            (printf "%-12s %-10s %-11s %4d %4d %4d %4d %4d  P %s R %s%n" name (clojure.core/name role) kind oracle predicted tp fp fn
                    (if (pos? predicted) (format "%.3f" p) "  —  ") (if (pos? oracle) (format "%.3f" r) "  —  "))
            (when residue?
              (doseq [k (take 10 fns)] (println "     missed   " k))
              (doseq [k (take 10 fps)] (println "     wrong    " k))))))))
    (println)
    (println "evidence, per rule, from the registry")
    (doseq [[rung rules] (sort-by key (group-by :evidence (sift/rules (sift/linter {}))))]
      (printf "  %-10s %3d  %s%n" (clojure.core/name rung) (count rules)
              (if (= rung :unjudged) (str/join " " (sort (map (comp #(subs % 1) str :id) rules))) "")))
    (println)
    (println "the gate is the suite: bb test")))
