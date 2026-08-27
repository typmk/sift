(ns com.typemark.sift.typeflow
  "Where a tag comes from, how far it travels, and where it dies — predicted
  from the source, for any host, and judged against the host compiler.

  Clojure on a host is an overlay: every value is the host's Object, every
  function is IFn.invoke(Object…), and the only static types are hints,
  primitives, literals and host signatures. The compiler carries a tag
  locally — a hinted param, a literal, a cast, a core fn it knows — and
  where the tag runs out it takes the slow path and says so:
  `*warn-on-reflection*` and `:warn-on-boxed` on the JVM, `:infer-warning`
  from Closure. Those warnings are the oracle (assay reads the JVM's,
  defnet's op=hostwarn reads Closure's) and this is the prediction.

  Dialyzer's stance, not core.typed's: nothing is annotated, and the
  claim is only ever `the compiler will NOT know this tag here` — a
  success-typing claim about a failure, never `this program is well-typed`.
  Typed Clojure's runtime-trace inference and Typed Racket's occurrence
  typing are the ancestors; the host table in hosts.edn is what makes a
  dialect a row rather than a namespace, and concepts.edn is the lattice
  above the host so a literal `{}` and a hint `^IPersistentMap` meet.

  Intra-definition, forward, one pass: params (hinted or Object), literals,
  `let`/`loop` bindings, casts, core fns the host table knows, and
  `:arith` — primitive iff every operand is primitive. Anything else is
  Object. Predictions:

    :boxed-math   (jvm) a math op with an operand whose tag is not primitive
    :reflection   (jvm) an instance call or field on a receiver whose tag is
                  not known
    :uninferred   (js)  the same, on a receiver without ^js or a js/ global

  Measured against assay's notes over this library's own source before it
  was believed — the numbers are in the README, and they are the claim."
  (:require [clojure.string :as str]
            [com.typemark.sift.zip :refer [children peel list-op op-name head-name
                                           collect pos-of sexpr token-name
                                           binder-vec vec-pairs]]
            #?(:clj  [com.typemark.sift.data-rules :refer [load-edn]]
               :cljs [com.typemark.sift.data-rules :refer-macros [load-edn]])
            [rewrite-clj.node :as n]
            [rewrite-clj.zip :as z]))

(def hosts (load-edn "hosts.edn"))
(def concepts (load-edn "concepts.edn"))

(def ^:private jvm-carrier->concept
  (into {} (for [{:keys [id jvm]} (:concepts concepts) :when jvm] [jvm id])))

(defn host-of
  "Which host a file compiles for: .cljs -> :js, else :jvm. A .cljc is
  scored for the JVM; a dialect that wants otherwise passes `host`."
  [path]
  (if (and path (str/ends-with? path ".cljs")) :js :jvm))

;; ---- tags ---------------------------------------------------------------
;;
;; A tag is a string (a host class or primitive), a keyword (a concept from
;; concepts.edn — :map, :vector — which is known but not primitive), or nil
;; for Object/unknown.

(defn- head*
  "The head name of a list OR a `#(…)` — zip/head-name answers nil for a
  :fn node, and `#(inc (:n %))` is a math op whose head is `inc`."
  [c]
  (if (= :fn (z/tag c))
    (some-> (children c) first token-name)
    (head-name c)))

(defn- head-full
  "The head symbol's full text — `java.security.MessageDigest/getInstance`,
  where `head*` gives only `getInstance`. Interop is recognised on this."
  [c]
  (let [h (if (= :fn (z/tag c)) (some-> (children c) first) (some-> (children c) first))]
    (when h
      (let [s (sexpr (peel h) ::no)]
        (when (symbol? s) (str s))))))

(defn- primitive? [host tag] (contains? (get-in hosts [host :primitives]) tag))
(defn- known? [tag] (some? tag))

(defn- hint-of
  "`^String s`, `^{:tag String} s`, `^js x` -> the tag text, or nil."
  [zloc]
  (when (and zloc (= :meta (z/tag zloc)))
    (let [[m] (children zloc)
          form (sexpr m ::no)]
      (cond
        (symbol? form) (name form)
        (keyword? form) (name form)
        (map? form) (some-> (or (:tag form) (get form 'tag)) str)
        :else nil))))

(defn- literal-tag [host zloc]
  (let [c (peel zloc)
        t (z/tag c)
        lit (get-in hosts [host :literal])]
    (case t
      :vector (:vector lit)
      :map (:map lit)
      :set (:set lit)
      :regex (:regex lit)
      :token (let [s (sexpr c ::no)]
               (cond
                 (= ::no s) nil
                 (nil? s) (:nil lit)
                 (string? s) (:string lit)
                 (keyword? s) (:keyword lit)
                 (boolean? s) (:bool lit)
                 (char? s) (:char lit)
                 (integer? s) (:int lit)
                 (number? s) (:float lit)
                 :else nil))
      nil)))

(defn- param-env
  "[^String s ^long n m] -> {\"s\" \"String\" \"n\" \"long\" \"m\" nil}."
  [argv]
  (into {}
        (for [p (children argv)
              :let [nm (token-name p)]
              :when (and nm (not= "&" nm))]
          [nm (hint-of p)])))

(declare tag-of bind-env interop-kind receiver-known?)

(defn- arith-tag [host env args]
  (let [tags (map #(tag-of host env %) args)]
    (if (and (seq tags) (every? #(primitive? host %) tags))
      (if (some #{"double" "float"} tags) "double" "long")
      "Number")))

(defn- tag-of
  "The tag the compiler would carry for this form, or nil."
  [host env zloc]
  (let [c (peel zloc)]
    (or (hint-of zloc)
        (literal-tag host c)
        (when (= :token (z/tag c))
          (when-let [nm (token-name c)]
            (get env nm)))
        (when (or (z/list? c) (= :fn (z/tag c)))
          (let [h (head* c)
                hosts-core (get-in hosts [host :core])
                casts (get-in hosts [host :casts])
                args (rest (children c))]
            (cond
              (nil? h) nil
              (contains? casts h) (get casts h)
              ;; the host compiler resolves a static call, a constructor, or
              ;; an instance call on a KNOWN receiver, and then knows the
              ;; result's class: "host" is known-but-unnamed. Measured on
              ;; agentia: (.digest (MessageDigest/getInstance …)) does not
              ;; reflect; (.getBytes (minify text)) does.
              ;; a host member the table knows returns a primitive or a class
              (get-in hosts [host :host-returns (head-full c)])
              (get-in hosts [host :host-returns (head-full c)])
              (get-in hosts [host :host-returns h])
              (get-in hosts [host :host-returns h])
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :static])))) "host"
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :ctor])))) "host"
              ;; (doto x …) is x
              (= "doto" h) (some->> (second (children c)) (tag-of host env))
              (and (interop-kind host h) (some->> (second (children c)) (receiver-known? host env))) "host"
              (= :arith (get hosts-core h)) (arith-tag host env args)
              (contains? hosts-core h) (get hosts-core h)
              ;; (let [...] body) / (do ... body): the last form's tag
              (contains? #{"let" "let*" "do" "when" "when-not"} h)
              (let [env' (if (contains? #{"let" "let*"} h) (bind-env host env (second (children c))) env)]
                (some->> (children c) last (tag-of host env')))
              :else nil))))))

(defn- bind-env
  "Extend `env` with a binding vector's names, each tagged by its hint or
  its init's tag."
  [host env bvec]
  (if (and bvec (z/vector? bvec))
    (reduce (fn [e [lhs rhs]]
              (if-let [nm (token-name lhs)]
                (assoc e nm (or (hint-of lhs) (tag-of host e rhs)))
                e))
            env (vec-pairs bvec))
    env))

;; ---- predictions ---------------------------------------------------------

(defn- interop-kind [host h]
  (when h
    (let [{:keys [instance-call field]} (get-in hosts [host :interop])]
      (cond (re-find (re-pattern field) h) :field
            (re-find (re-pattern instance-call) h) :instance-call
            :else nil))))

(defn- receiver-known? [host env zloc]
  (let [t (tag-of host env zloc)
        nm (token-name (peel zloc))]
    (or (known? t)
        (and nm (str/starts-with? nm "js/"))
        (and nm (contains? (get-in hosts [host :known-receiver]) (hint-of zloc))))))

(defn- predictions-in
  "Walk one definition body with `env`, extending it through let/loop and
  fn params, emitting one prediction per math op or interop call whose
  tag the compiler will not have."
  [host env zloc]
  (let [out (volatile! [])
        math (get-in hosts [host :math])
        warns (get-in hosts [host :warns])
        emit! (fn [kind c detail]
                (when (contains? warns kind)
                  (let [[line col] (or (pos-of c) [nil nil])
                        ;; the compiler positions a #(…) at its `(`, one past
                        ;; the `#` rewrite-clj reports — measured against assay
                        col (if (and col (= :fn (z/tag c))) (inc col) col)]
                    (vswap! out conj (merge {:kind kind :line line :column col} detail)))))]
    (letfn [(walk-binds [env bvec]
              ;; inits and :when/:let/:while forms are code too, and the
              ;; oracle's first false negatives were exactly there
              (when (and bvec (z/vector? (peel bvec)))
                (reduce (fn [e [lhs rhs]]
                          (walk e rhs)
                          (if-let [nm (token-name lhs)]
                            (assoc e nm (or (hint-of lhs) (tag-of host e rhs)))
                            e))
                        env (vec-pairs (peel bvec)))))
            (walk [env c]
              (let [c (peel c)]
                (cond
                  (nil? c) nil
                  ;; a vector, map or set literal holds code — the first
                  ;; misses were math inside a map literal's values
                  (contains? #{:vector :map :set} (z/tag c))
                  (doseq [k (children c)] (walk env k))
                  (or (z/list? c) (= :fn (z/tag c)))
                  (let [h (head* c)
                        kids (children c)]
                    (cond
                      (contains? #{"let" "let*" "loop" "doseq" "for" "dotimes" "with-open"
                                   "if-let" "when-let" "if-some" "when-some" "when-first"} h)
                      (let [env' (or (walk-binds env (second kids)) env)]
                        (doseq [k (drop 2 kids)] (walk env' k)))

                      ;; (doto x (.a) (.b)) / (-> x (.a) (.b)) / (.. x a b): the
                      ;; receiver of each member form is the threaded value,
                      ;; not its first argument. Measured on lume: every
                      ;; (doto (HikariConfig.) (.setJdbcUrl …)) was a false
                      ;; positive until this.
                      (contains? #{"doto" "->" "->>" ".."} h)
                      (let [target (second kids)
                            known? (some->> target (receiver-known? host env))
                            steps (drop 2 kids)]
                        (walk env target)
                        (doseq [st steps]
                          (let [st (peel st)]
                            (if (and (or (z/list? st) (= :fn (z/tag st))) (interop-kind host (head* st)))
                              ;; a member step: judge on the threaded receiver, walk its args
                              (do (when-not known?
                                    (emit! (if (= host :js) :uninferred :reflection) st
                                           {:op (head* st) :interop (interop-kind host (head* st))
                                            :receiver (some-> target peel z/string)}))
                                  (doseq [k (rest (children st))] (walk env k)))
                              (walk env st)))))

                      (contains? #{"fn" "fn*" "defn" "defn-" "defmethod"} h)
                      (let [argv (first (filter #(z/vector? (peel %)) kids))
                            env' (if argv (merge env (param-env (peel argv))) env)]
                        (doseq [k kids :when (not= k argv)] (walk env' k)))

                      (contains? #{"catch"} h)
                      (let [[_ cls b & body] kids
                            env' (if-let [nm (token-name b)] (assoc env nm (some-> cls token-name)) env)]
                        (doseq [k body] (walk env' k)))

                      :else
                      (do
                        (when (and h (contains? math h))
                          (let [tags (map #(tag-of host env %) (rest kids))]
                            (when-not (every? #(primitive? host %) tags)
                              (emit! :boxed-math c {:op h :tags (vec tags)}))))
                        (when-let [ik (interop-kind host h)]
                          (when-let [recv (second kids)]
                            (when-not (receiver-known? host env recv)
                              (emit! (if (= host :js) :uninferred :reflection) c
                                     {:op h :interop ik :receiver (some-> recv peel z/string)}))))
                        ;; a constructor or static call resolves an OVERLOAD, and
                        ;; an argument the compiler cannot type — Object, or a
                        ;; boxed Number from arithmetic — is a reflective call:
                        ;; (Date. (+ (System/currentTimeMillis) ttl)) and
                        ;; (OutputStreamWriter. stream "UTF-8") on lume, measured
                        (when (and (= host :jvm)
                                   (let [hf (head-full c)]
                                     (and hf (contains? (get-in hosts [host :overloaded]) hf))))
                          (let [tags (map #(tag-of host env %) (rest kids))]
                            (when (some #(or (nil? %) (= "Number" %)) tags)
                              (emit! :reflection c {:op h :interop :overload :tags (vec tags)}))))
                        (doseq [k kids] (walk env k))))))))]
      (walk env zloc)
      @out)))

(defn- defn-forms [zloc]
  (collect zloc (fn [c] (contains? #{"defn" "defn-" "defmethod" "defmacro"} (head-name c)))))

(defn predictions
  "Source -> [{:kind :line :column :op :tags/:receiver :in \"name\"} …] for
  the host `path` compiles for."
  ([text path] (predictions text path (host-of path)))
  ([text path host]
   (let [zloc (try (z/of-string text {:track-position? true})
                   (catch #?(:clj Exception :cljs :default) _ nil))
         zloc (when zloc (z/up zloc))]
     (if-not zloc
       []
       (vec (for [d (defn-forms zloc)
                  :let [nm (some-> (children d) second token-name)]
                  p (predictions-in host {} d)]
              (assoc p :in nm :file path)))))))

(defn findings
  "Predictions as findings, one rule per kind under :typeflow/."
  [text path]
  (for [{:keys [kind line column op tags receiver in]} (predictions text path)]
    {:rule (keyword "typeflow" (name kind))
     :line line :column column
     :symbol (some-> in symbol)
     :message (case kind
                :boxed-math (str op " over " (str/join ", " (map #(or % "Object") tags)) " boxes; hint or cast the operands")
                :reflection (str op " on " receiver " reflects; its tag is not known here")
                :uninferred (str op " on " receiver ": Closure cannot infer the target; hint ^js or use a js/ global"))
     :applicability :unspecified}))
