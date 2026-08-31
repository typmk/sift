(ns com.typemark.sift.wrap
  "SBCL's `trace`, as a library loaded into the target process — the one
   producer of defnet's trace format that must run in someone else's image,
   which is why it lives here beside the oracle and not in defnet.

   Wrap a var and every call writes one JSON line to the trace file:

     {\"call\": [caller, subject], \"subject\": \"ns/name\",
      \"args\": [{\"slot\": 0, \"type\": \"Double\"}], \"ret\": \"Double\",
      \"ms\": 0.12, \"ctx\": \"main\"}

   defnet `ingest op=trace` reads exactly this; the format (ingest/trace.cljs)
   is the contract, and wrap_test holds this side of it. The caller comes from
   the stack — the first frame outside this namespace and clojure.* that
   demunges to a Clojure function. A call that throws still writes a line,
   with {\"metrics\": {\"throws\": 1}} and no ret.

   Every line is flushed as written, so a crashed process keeps what it saw.
   That is a cost per call; this is an observer, not a profiler.

     (require '[com.typemark.sift.wrap :as w])
     (w/start! \"trace.jsonl\")
     (w/wrap-ns! 'my.app.core)   ; or (w/wrap-var! #'my.app.core/step)
     ;; … exercise the program …
     (w/stop!)                    ; unwraps everything, closes the file"
  (:require [clojure.string :as str])
  (:import [java.io Writer FileWriter BufferedWriter]))

(defonce ^:private out (atom nil))
(defonce ^:private wrapped (atom {}))

(defn- json-str ^String [x]
  (cond
    (nil? x) "null"
    (string? x) (str \" (-> x (str/replace "\\" "\\\\") (str/replace "\"" "\\\"")) \")
    (keyword? x) (json-str (name x))
    (number? x) (str x)
    (map? x) (str "{" (str/join "," (map (fn [[k v]] (str (json-str k) ":" (json-str v))) x)) "}")
    (sequential? x) (str "[" (str/join "," (map json-str x)) "]")
    :else (json-str (str x))))

(defn- emit! [m]
  (when-let [{:keys [writer]} @out]
    (locking writer
      (.write ^Writer writer (json-str m))
      (.write ^Writer writer "\n")
      (.flush ^Writer writer))))

(defn- caller-name
  "The nearest stack frame that is a Clojure function outside this namespace
   and clojure.*, as ns/name — or nil, which drops the `call` key rather than
   guessing, the same rule every defnet reader follows."
  []
  (some (fn [^StackTraceElement el]
          (let [cn (.getClassName el)]
            (when (and (str/includes? cn "$")
                       (not (str/starts-with? cn "com.typemark.sift.wrap$"))
                       (not (str/starts-with? cn "clojure.")))
              (let [[nsp nm] (str/split (clojure.lang.Compiler/demunge cn) #"/" 3)]
                (when (and nsp nm)
                  (str nsp "/" (first (str/split nm #"--"))))))))
        (.getStackTrace (Thread/currentThread))))

(defn- type-name [v]
  (if (nil? v) "nil" (.getSimpleName (class v))))

(defn start!
  "Open PATH for appending; every wrapped call from now on writes to it."
  [path]
  (reset! out {:writer (BufferedWriter. (FileWriter. (str path) true))
               :path (str path)})
  path)

(defn wrap-var!
  "Wrap one var. Returns its qualified name, or nil when the var holds no
   function, is a macro, or is already wrapped."
  [v]
  (let [m (meta v)
        subject (str (ns-name (:ns m)) "/" (:name m))]
    (when (and (not (:macro m)) (fn? @v) (not (contains? @wrapped v)))
      (let [orig @v
            wrapper
            (fn [& args]
              (let [caller (caller-name)
                    base (cond-> {:subject subject
                                  :args (vec (map-indexed
                                              (fn [i a] {:slot i :type (type-name a)})
                                              args))
                                  :ctx (.getName (Thread/currentThread))}
                           caller (assoc :call [caller subject]))
                    t0 (System/nanoTime)]
                (try
                  (let [ret (apply orig args)]
                    (emit! (assoc base
                                  :ret (type-name ret)
                                  :ms (/ (- (System/nanoTime) t0) 1e6)))
                    ret)
                  (catch Throwable t
                    (emit! (assoc base
                                  :ms (/ (- (System/nanoTime) t0) 1e6)
                                  :metrics {:throws 1}))
                    (throw t)))))]
        (swap! wrapped assoc v orig)
        (alter-var-root v (constantly wrapper))
        subject))))

(defn wrap-ns!
  "Wrap every public function var in NS-SYM. Returns the wrapped names."
  [ns-sym]
  (require ns-sym)
  (vec (keep wrap-var! (vals (ns-publics ns-sym)))))

(defn unwrap-all!
  "Restore every wrapped var. Returns how many."
  []
  (let [w @wrapped]
    (doseq [[v orig] w]
      (alter-var-root v (constantly orig)))
    (reset! wrapped {})
    (count w)))

(defn stop!
  "Unwrap everything and close the file."
  []
  (let [n (unwrap-all!)]
    (when-let [{:keys [writer]} @out]
      (locking writer (.close ^Writer writer)))
    (reset! out nil)
    n))
