(ns net.typemark.sift.oracle
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as sh]))

(def warning-re #"(?m)^(Reflection|Boxed math) warning, (.*):(\d+):(\d+) - (.*)$")
(def call-re #"([\w.$]+)\.(\w+)\(([^)]*)\)\s*\.?\s*$")
(def primitive '#{long double int float boolean char byte short})

(defn- simple [t] (last (str/split t #"\.")))

(defn- taken-refused
  [detail]
  (when-let [[_ cls method params] (re-find call-re detail)]
    (let [taken (mapv simple (remove str/blank? (str/split params #",")))
          ms (try (filter #(= method (.getName ^java.lang.reflect.Method %))
                          (.getDeclaredMethods (Class/forName cls)))
                  (catch Throwable _ nil))
          sig (fn [^java.lang.reflect.Method m] (mapv #(.getSimpleName ^Class %) (.getParameterTypes m)))]
      (when (seq ms)
        #:assay.note{:taken (str method "(" (str/join "," taken) ")")
                     :refused (vec (sort (for [m ms :let [ps (sig m)]
                                               :when (and (not= ps taken) (= (count ps) (count taken))
                                                          (every? (comp primitive symbol) ps))]
                                           (str method "(" (str/join "," ps) ")->"
                                                (.getSimpleName (.getReturnType ^java.lang.reflect.Method m))))))}))))

(defn- notes-in [text]
  (for [[_ kind file line col detail] (re-seq warning-re text)
        :let [code (if (= kind "Reflection") :assay.note/reflection :assay.note/boxed-math)]]
    (merge #:assay.note{:code code
                        :severity (if (= code :assay.note/reflection) :assay.severity/warning :assay.severity/note)
                        :span {:assay/file (when-not (= file "NO_SOURCE_PATH") file)
                               :assay/line (parse-long line)
                               :assay/col (parse-long col)}
                        :message (str/replace detail #"\.$" "")}
           (when (= code :assay.note/boxed-math) (taken-refused detail)))))

(defn -main [& argv]
  (let [args (vec argv)
        out (or (some->> args (drop-while #(not= "--out" %)) second) "oracle")
        roots (or (seq (remove #{"--out" out} args)) ["src"])
        _ (.mkdirs (io/file out))
        files (for [r roots f (file-seq (io/file r))
                    :when (and (.isFile ^java.io.File f) (re-find #"\.cljc?$" (str f)))]
                f)
        read-ns (fn [^java.io.File f]
                  (try (with-open [rdr (java.io.PushbackReader. (io/reader f))]
                         (let [form (read {:read-cond :allow :eof nil} rdr)]
                           (when (and (seq? form) (= 'ns (first form))) form)))
                       (catch Throwable _ nil)))
        ns-forms (keep read-ns files)
        tail2 (fn [f] (str/join "/" (take-last 2 (str/split (str f) #"/"))))]
    (let [acc (atom [])
          loaded (doall (for [f files :let [form (read-ns f) n (second form)] :when n
                              :let [err (java.io.StringWriter.)
                                    ok (binding [*warn-on-reflection* true
                                                 *unchecked-math* :warn-on-boxed
                                                 *err* (java.io.PrintWriter. err true)]
                                         (try (require n) true
                                              (catch Throwable e
                                                (binding [*out* *err*] (println "not loaded:" n "-" (.getMessage e)))
                                                false)))
                                    _ (swap! acc into (notes-in (str err)))]
                              :when ok]
                          (tail2 f)))
          notes (distinct @acc)]
      (spit (str out "/notes.edn") (with-out-str (run! prn notes)))
      (spit (str out "/loaded.edn") (with-out-str (prn (vec loaded))))
      (binding [*out* *err*] (println (count files) "files," (count loaded) "loaded," (count notes) "notes")))
    (let [nses (->> ns-forms (mapcat (fn [form] (for [c (rest form) :when (and (seq? c) (= :require (first c))) spec (rest c)]
                                                 (cond (symbol? spec) spec (sequential? spec) (first spec)))))
                    (remove nil?) (cons 'clojure.core) (concat (map second ns-forms)) distinct)
          vars (into (sorted-map)
                     (for [n nses
                           :let [ok (try (require n) (some? (find-ns n)) (catch Throwable _ false))]
                           :when ok
                           [s v] (ns-interns n)
                           :let [m (meta v)
                                 t (or (:tag m) (some-> (:arglists m) first meta :tag)
                                       (when (and (:const m) (bound? v))
                                         (let [x @v] (cond (instance? Long x) 'long (instance? Double x) 'double
                                                           (string? x) 'String (boolean? x) 'boolean :else nil))))]
                           :when t]
                       [(str n "/" s) (str (if (class? t) (.getName ^Class t) t))]))
          imports (for [form ns-forms c (rest form) :when (and (seq? c) (= :import (first c))) spec (rest c)
                        k (cond (symbol? spec) [spec] (sequential? spec) (map #(symbol (str (first spec) "." %)) (rest spec)) :else [])]
                    (str k))
          by-simple (into {} (map (fn [c] [(last (str/split c #"\.")) c]) imports))
          texts (map slurp files)
          named (concat (for [t texts h (re-seq #"\^([A-Za-z][A-Za-z0-9_.]*)" t)] (second h))
                        (for [t texts h (re-seq #"[(\s\[]([A-Z][A-Za-z0-9_$]*)/[a-zA-Z]" t)] (second h))
                        (for [t texts h (re-seq #"\(([A-Z][A-Za-z0-9_.$]*)\.[\s)]" t)] (second h))
                        (for [t texts h (re-seq #"\(reify\s+([A-Za-z][A-Za-z0-9_.$]*)" t)] (second h))
                        (for [t texts h (re-seq #"\(catch\s+([A-Za-z][A-Za-z0-9_.$]*)" t)] (second h)))
          resolve-name (fn [h] (or (get by-simple h)
                                   (when-let [[_ outer inner] (re-matches #"([^$.]+)\$(.+)" h)]
                                     (some-> (get by-simple outer) (str "$" inner)))
                                   (if (str/includes? h ".") h (str "java.lang." h))))
          literal-classes ["clojure.lang.PersistentVector" "clojure.lang.IPersistentVector" "clojure.lang.PersistentArrayMap"
                           "clojure.lang.PersistentHashMap" "clojure.lang.IPersistentMap" "clojure.lang.PersistentHashSet"
                           "clojure.lang.IPersistentSet" "clojure.lang.PersistentList" "clojure.lang.ISeq" "clojure.lang.AFunction"
                           "clojure.lang.Keyword" "clojure.lang.Symbol" "java.lang.String" "java.lang.Long" "java.lang.Double"
                           "java.lang.Boolean" "java.lang.Number" "java.lang.Character" "java.util.regex.Pattern" "java.lang.Object"]
          load (fn [cn] (try (Class/forName cn) (catch Throwable _ nil)))
          seed (->> (concat imports (map resolve-name named) (filter #(str/includes? % ".") (vals vars)) literal-classes) distinct (keep load))
          returns-of (fn [^Class c] (for [^java.lang.reflect.Method m (.getMethods c)
                                          :when (java.lang.reflect.Modifier/isPublic (.getModifiers m))
                                          :let [r (.getReturnType m)]
                                          :when (and (not (.isPrimitive r)) (not (.isArray r)) (not= Object r) (not (str/starts-with? (.getName r) "java.lang.")))]
                                      r))
          classes (loop [acc (vec seed) seen (set seed) frontier seed n 0]
                    (let [nxt (->> frontier (mapcat returns-of) distinct (remove seen))]
                      (if (or (empty? nxt) (> n 4) (> (count acc) 3000))
                        acc
                        (recur (into acc nxt) (into seen nxt) nxt (inc n)))))
          sname (fn [^Class c] (if (.isArray c) (str (.getSimpleName (.getComponentType c)) "[]")
                                   (last (clojure.string/split (.getName c) #"\."))))
          supers (fn [^Class c] (loop [acc #{} q [c]]
                                  (if-let [x (first q)]
                                    (let [nxt (concat (when-let [s (.getSuperclass ^Class x)] [s]) (.getInterfaces ^Class x))]
                                      (recur (into acc (map sname nxt)) (concat (rest q) nxt)))
                                    (vec (sort acc)))))
          entry (fn [^Class c]
                  {:supers (supers c)
                   :fields (into (sorted-map) (for [^java.lang.reflect.Field f (.getFields c)] [(.getName f) (sname (.getType f))]))
                   :ctors (vec (for [^java.lang.reflect.Constructor k (.getConstructors c)]
                                 {:params (mapv sname (.getParameterTypes k))}))
                   :methods (into (sorted-map)
                                  (for [[n ms] (group-by #(.getName ^java.lang.reflect.Method %)
                                                         (filter #(java.lang.reflect.Modifier/isPublic (.getModifiers ^java.lang.reflect.Method %)) (.getMethods c)))]
                                    [n (vec (for [^java.lang.reflect.Method m ms
                                                  :let [pub? (java.lang.reflect.Modifier/isPublic (.getModifiers (.getDeclaringClass m)))
                                                        base? (or pub?
                                                                  (boolean (some (fn [^Class b] (and (java.lang.reflect.Modifier/isPublic (.getModifiers b))
                                                                                                     (try (.getMethod b (.getName m) (.getParameterTypes m)) true (catch Throwable _ false))))
                                                                                 (loop [acc [] q [(.getDeclaringClass m)]]
                                                                                   (if-let [x (first q)]
                                                                                     (let [nxt (concat (when-let [sc (.getSuperclass ^Class x)] [sc]) (.getInterfaces ^Class x))]
                                                                                       (recur (into acc nxt) (concat (rest q) nxt)))
                                                                                     acc)))))]]
                                              (cond-> {:params (mapv sname (.getParameterTypes m))
                                                       :returns (sname (.getReturnType m))
                                                       :static? (java.lang.reflect.Modifier/isStatic (.getModifiers m))}
                                                (not base?) (assoc :public? false))))]))})
          table (into (sorted-map) (for [^Class c classes] [(.getName c) (entry c)]))
          by-simple (reduce (fn [m ^Class c] (update m (sname c) (fnil conj []) (.getName c))) (sorted-map) classes)]
      (spit (str out "/tags.edn") (pr-str {:vars vars :classes table :by-simple by-simple}))
      (binding [*out* *err*] (println (count vars) "tagged vars," (count table) "classes")))
    (let [r (try (apply sh/sh "clj-kondo" "--lint" (concat roots ["--config" "{:analysis {:arglists true :locals true} :output {:format :json}}"]))
                 (catch Throwable _ nil))]
      (if (and r (seq (:out r)))
        (do (spit (str out "/analysis.json") (:out r)) (binding [*out* *err*] (println "analysis.json written")))
        (binding [*out* *err*] (println "clj-kondo not on PATH; no analysis.json"))))
    (shutdown-agents)
    (System/exit 0)))
