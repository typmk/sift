(ns net.typemark.sift.cli
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [net.typemark.sift :as sift]
            [net.typemark.sift.resolve :as resolve]
            [net.typemark.sift.baseline :as baseline]
            [net.typemark.sift.complexity :as cx]))

(defn- root
  []
  (-> (io/resource "net/typemark/sift/cli.clj") io/file fs/parent fs/parent fs/parent fs/parent fs/parent str))

(defn- clj-file? [p] (contains? #{"clj" "cljs" "cljc"} (fs/extension p)))

(defn- expand [path]
  (let [p (fs/path path)]
    (cond (fs/regular-file? p) (if (clj-file? p) [p] [])
          (fs/directory? p) (filterv clj-file? (fs/glob p "**.{clj,cljs,cljc}"))
          :else (throw (ex-info (str "not a file or directory: " path) {:path (str path)})))))

(defn- print-finding [{:keys [file line column rule message instruction counterpart applicability category evidence]}]
  (printf "%s:%s:%s %s [%s, %s] %s\n" file line (or column 1) (name rule)
          (name (or category :unknown)) (name (or evidence :unjudged)) message)
  (when (and instruction (not= instruction message)) (printf "  fix: %s\n" instruction))
  (when counterpart (printf "  counterpart (%s):\n    %s\n" (name applicability) (pr-str counterpart))))

(defn- usage []
  (binding [*out* *err*]
    (println "usage: sift (lint | complexity) [--edn] [--category C] [--threshold N] [--analysis A | --oracle DIR] [--vocabulary V] [--baseline B | --write-baseline B] <path> ...  |  sift oracle [--js] [--out DIR] ..."))
  (System/exit 2))

(defn- externs!
  [out jar]
  (let [jar (or jar
                (->> (fs/glob (str (System/getProperty "user.home") "/.m2/repository/com/google/javascript/closure-compiler") "**/closure-compiler-v*.jar")
                     (map str) (remove #(str/includes? % "sources")) sort last))
        _ (when-not jar (binding [*out* *err*] (println "no closure-compiler jar found; pass one")) (System/exit 2))
        inner (str (fs/create-temp-dir) "/externs.zip")]
    (with-open [zip (java.util.zip.ZipFile. (str jar))
                in (.getInputStream zip (.getEntry zip "externs.zip"))
                o (io/output-stream inner)]
      (io/copy in o))
    (with-open [z (java.util.zip.ZipFile. inner)]
      (let [names (into (sorted-set)
                        (for [e (enumeration-seq (.entries z)) :when (str/ends-with? (.getName e) ".js")
                              [_ p] (re-seq #"\.([A-Za-z_$][A-Za-z0-9_$]*)" (slurp (.getInputStream z e)))]
                          p))]
        (fs/create-dirs out)
        (spit (str out "/externs.edn") (pr-str names))
        (binding [*out* *err*] (println (count names) "property names from" (fs/file-name jar) "->" (str out "/externs.edn")))))))

(defn- oracle!
  [args]
  (let [args (vec args)]
    (if (some #{"--js"} args)
      (let [out (or (some->> args (drop-while #(not= "--out" %)) second) "oracle")
            jar (first (remove (into #{"--js" "--out"} [out]) args))]
        (externs! out jar)
        (System/exit 0))
      (System/exit (:exit @(process/process (into ["clojure" "-Sdeps" (pr-str {:deps {'net.typemark/sift-oracle {:local/root (str (root) "/oracle")}}})
                                                   "-M" "-m" "net.typemark.sift.oracle"]
                                                  args)
                                            {:inherit true}))))))

(defn- run-cli!
  [args]
  (let [args      args
        cmd       (first args)
        rest-args (rest args)
        edn?      (some #{"--edn"} rest-args)
        threshold (some->> rest-args (drop-while #(not= "--threshold" %)) second parse-long)
        odir      (some->> rest-args (drop-while #(not= "--oracle" %)) second)
        vocab-file (some->> rest-args (drop-while #(not= "--vocabulary" %)) second)
        vocabulary (when vocab-file (edn/read-string (slurp vocab-file)))
        analysis  (or (some->> rest-args (drop-while #(not= "--analysis" %)) second)
                      (when odir (let [f (str odir "/analysis.json")] (when (fs/exists? f) f))))
        resolution (when analysis (resolve/index (slurp analysis)))
        prose     (when analysis (sift/prose-findings (slurp analysis) vocabulary))
        dump      (when odir (let [f (str odir "/tags.edn")] (when (fs/exists? f) (edn/read-string (slurp f)))))
        loaded    (when odir (let [f (str odir "/loaded.edn")] (when (fs/exists? f) (set (edn/read-string (slurp f))))))
        base-file (some->> rest-args (drop-while #(not= "--baseline" %)) second)
        write-base (some->> rest-args (drop-while #(not= "--write-baseline" %)) second)
        category  (some->> rest-args (drop-while #(not= "--category" %)) second keyword)
        paths     (->> rest-args
                       (remove #(str/starts-with? % "--"))
                       (remove #(and threshold (= % (str threshold))))
                       (remove #(and analysis (= % analysis)))
                       (remove #(and vocab-file (= % vocab-file)))
                       (remove #(and odir (= % odir)))
                       (remove #(and base-file (= % base-file)))
                       (remove #(and write-base (= % write-base)))
                       (remove #(and category (= % (name category))))
                       vec)
        tenanted  (delay (sift/tenanted? (map (comp slurp str (fn [p] (fs/absolutize p))) (mapcat expand paths))))
        run       (case cmd
                    "lint"       (fn [file]
                                   (let [{:keys [ok? error findings]} (sift/analyze {:text (slurp file) :path file :resolution resolution
                                                                                     :var-tags (:vars dump) :classes dump :loaded loaded
                                                                                     :prose prose :vocabulary vocabulary
                                                                                     :tenanted? @tenanted})]
                                     (if-not ok?
                                       [{:file file :line 1 :column 1 :rule :unparseable :message error}]
                                       (cond->> (map #(assoc % :file file) findings)
                                         category (filter #(= category (:category %)))))))
                    "complexity" (fn [file] (map #(assoc % :file file)
                                                 (cx/findings (slurp file) file (or threshold cx/default-threshold))))
                    (usage))]
    (when (empty? paths) (usage))
    (let [rel (fn [f] (str (fs/relativize (fs/cwd) f)))
          fs (vec (mapcat (fn [p] (map #(assoc % :file (rel (:file %)))
                                       (run (str (fs/absolutize p)))))
                          (mapcat expand paths)))
          fs (if base-file
               (baseline/new-findings (baseline/parse (slurp base-file)) fs)
               fs)]
      (when write-base
        (spit write-base (baseline/render (baseline/counts fs)))
        (println (count fs) "findings written to" write-base)
        (System/exit 0))
      (if edn?
        (prn fs)
        (if (seq fs) (run! print-finding fs) (println "no findings")))
      (flush)
      (System/exit (if (seq fs) 1 0)))))

(defn -main [& args]
  (if (= "oracle" (first args))
    (oracle! (rest args))
    (run-cli! args)))
