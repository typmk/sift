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

(defn- host-branch
  "`#?(:clj A :cljs B)` -> A for the JVM, B for JS (`:default` either way);
  nil when the node is not a reader conditional. The catch class in
  `(catch #?(:clj Exception :cljs :default) e …)` was invisible, so `e` was
  unknown and every `.getMessage` on it a false reflection."
  [host zloc]
  (let [c (peel zloc)]
    (when (and c (= :reader-macro (z/tag c)))
      (let [[m body] (children c)]
        (when (and m (= "?" (z/string m)) body (z/list? body))
          (let [want (if (= host :js) :cljs :clj)
                pairs (partition 2 (children body))]
            (or (some (fn [[k v]] (when (= want (sexpr k ::no)) v)) pairs)
                (some (fn [[k v]] (when (= :default (sexpr k ::no)) v)) pairs))))))))
(defn- known? [tag] (some? tag))

(defn- hint-of
  "`^String s`, `^{:tag String} s`, `^js x` -> the tag text, or nil."
  [zloc]
  (when (and zloc (= :meta (z/tag zloc)))
    (let [[m] (children zloc)
          form (sexpr m ::no)]
      (cond
        (symbol? form) (name form)
        ;; ^js is a tag; ^:private and ^:const are not, and var-tags read
        ;; "private" as a return class until this said so
        (keyword? form) (when (= :js form) "js")
        (map? form) (some-> (or (:tag form) (get form 'tag)) str)
        :else nil))))

(defn- js-literal?
  "`#js {…}` / `#js […]` — an object the code built, whose keys are quoted
  and whose `.-k` reads are renamed by :advanced."
  [zloc]
  (let [c (peel zloc)]
    (or (and (= :reader-macro (z/tag c)) (= "js" (some-> (z/down c) z/string)))
        (contains? #{"clj->js" "js-obj"} (head-name c)))))

(defn- constant-coll?
  "Every element a literal (or a nested constant collection) — what the
  compiler folds into one constant."
  [c]
  (every? (fn [k] (let [k (peel k) t (z/tag k)]
                    (cond (contains? #{:vector :map :set} t) (constant-coll? k)
                          (= :token t) (let [s (sexpr k ::no)] (and (not= ::no s) (not (symbol? s))))
                          :else false)))
          (children c)))

(defn- literal-tag [host zloc]
  (let [c (peel zloc)
        t (z/tag c)
        lit (get-in hosts [host :literal])]
    (if (js-literal? c)
      "js-literal"
      (case t
      ;; ["sh" "-c"] is a ConstantExpr, a PersistentVector, a java.util.List;
      ;; ["gh" repo] is a VectorExpr whose class is IPersistentVector, which
      ;; is NOT a List — so (ProcessBuilder. …) resolves on one and reflects
      ;; on the other. Compiler.analyzeSeq/VectorExpr.parse, measured on
      ;; clojure-mcp against agentia.
      :vector (if (constant-coll? c) (:vector-const lit (:vector lit)) (:vector lit))
      :map (if (constant-coll? c) (:map-const lit (:map lit)) (:map lit))
      :set (if (constant-coll? c) (:set-const lit (:set lit)) (:set lit))
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
      nil))))

(defn- destructured
  "The symbols a binding form binds, each with its hint: a plain symbol, a
  vector, or a map with :keys / :syms / :strs and :as — {:keys [^Writer out
  ^BufferedReader in]} carries hints the compiler honours, and darling's
  proc-call was three false positives until they were read."
  [lhs]
  (let [c (peel lhs)]
    (cond
      (nil? c) []
      (token-name c) [[(token-name c) (hint-of lhs)]]
      (z/vector? c) (mapcat destructured (children c))
      (z/map? c) (mapcat (fn [[k v]]
                           (let [kw (sexpr (peel k) ::no)]
                             (cond (contains? #{:keys :syms :strs} kw) (mapcat destructured (children (peel v)))
                                   (= :as kw) (destructured v)
                                   (keyword? kw) []
                                   :else (destructured k))))
                         (partition 2 (children c)))
      :else [])))

(defn- param-env
  "[^String s ^long n m] -> {\"s\" \"String\" \"n\" \"long\" \"m\" nil} —
  through destructuring too."
  [argv]
  (into {}
        (for [p (children argv)
              [nm t] (destructured p)
              :when (and nm (not= "&" nm))]
          [nm t])))

(declare tag-of bind-env interop-kind receiver-known? defn-forms predictions* thread-tag)

(defn- arith-tag
  "primitive iff every operand is — except `/` over longs, which is
  Numbers.divide(long, long) and returns a Number (a Ratio, perhaps):
  (* 100.0 (/ (count a) (count b))) boxes the multiply, darling-toolkit."
  [host env op args]
  (let [tags (map #(tag-of host env %) args)]
    (cond
      ;; Numbers.add(double, Object) RETURNS double — it coerces the Object —
      ;; so a double operand makes the result double even beside an unknown,
      ;; and only the warning marks the boxing. kora color.clj:80-81:
      ;; (* 2 (/ l 100.0)) is a double and Math/abs over it resolves.
      ;; Numbers.add(long, Object) returns Number: the Object might be a double.
      (and (seq tags) (some #{"double" "float"} tags)) "double"
      (and (seq tags) (every? #(primitive? host %) tags)) (if (= "/" op) "Number" "long")
      :else "Number")))

(def ^:dynamic *var-tags*
  "{\"ns/name\" tag} — return tags of vars, from `var-tags` over the corpus's
  own trees and/or bin/var-tags on the JVM for libraries. nil is none."
  nil)

(def ^:dynamic *resolve*
  "{:vars {[row col] \"ns/name\"}} for the file being walked, from
  resolve/for-file, so a call form's head resolves to the var it names."
  nil)

(defn- var-return-tag
  "The tag a call to a user or library var returns, if kondo resolved the
  call and the var carries one. The one step a text pass cannot take alone."
  [c]
  (when (and *var-tags* *resolve*)
    (when-let [qn (get (:vars *resolve*) (pos-of c))]
      (get *var-tags* qn))))

(def ^:dynamic *ns-name*
  "The namespace of the file being walked, so a bare `max-record-bytes`
  finds its own file's entry in `*var-tags*` without kondo."
  nil)

(defn- resolved-var [c]
  (when *resolve* (get (:vars *resolve*) (pos-of c))))

(defn- core-head
  "`h` when the head is a core fn — unqualified, spelled `clojure.core/` or
  `cljs.core/`, or resolved there by kondo — else nil. `prometheus/inc` is
  not `inc`: measured on lume, two boxed-math false positives were a
  metrics library's `inc` read as arithmetic."
  [c h]
  (when h
    (let [core? #(re-find #"^(clojure|cljs)\.core/|^clojure\.java\.io/|^io/" %)
          qn (resolved-var c)
          hf (head-full c)]
      (cond qn (when (core? qn) h)
            ;; `/` is the division symbol, not a namespace separator
            (= "/" hf) h
            (and hf (str/includes? hf "/") (not (core? hf))) nil
            :else h))))

(defn- token-var-tag
  "A bare symbol naming a var with a known tag: `(def ^:const max-bytes 4096)`
  is inlined by the compiler as the literal, so its uses are `long`."
  [c nm]
  (when *var-tags*
    (or (some->> (resolved-var c) (get *var-tags*))
        (when *ns-name* (get *var-tags* (str *ns-name* "/" nm)))
        (when (str/includes? nm "/") (get *var-tags* nm)))))

(defn- simple-name [tag]
  (when (string? tag) (last (str/split tag #"\."))))

(def ^:dynamic *classes*
  "bin/oracle's tags.edn: {:classes {full-name {:supers :fields :ctors :methods}}
  :by-simple {simple [full …]}}. nil is none, and then a resolved call is
  \"host\": known, unnamed."
  nil)

(def ^:dynamic *externs*
  "The property names Closure's default externs already declare — a set,
  from bin/externs. shadow-cljs's :infer-externs :auto warns for a member
  access on an untyped target ONLY when the property is not among them:
  (.beginPath ctx) on an untyped ctx is silent because every browser extern
  has beginPath, and (.-sameNs d) is not. Measured on the viewer at 92270f9^:
  the model without this set predicted 262 for 42, all 42 among them."
  nil)

(def ^:dynamic *imports*
  "{simple full} from the file's ns :import — how a simple name with two
  classes behind it (lume: java.util.Date and java.sql.Date) is resolved,
  as the compiler resolves it."
  {})

(defn- class-entry
  "The dumped class a tag names: by full name, else by simple name when
  one class carries it, else the one this file imports; nil when ambiguous."
  [tag]
  (when (and *classes* (string? tag))
    (let [t (if (keyword? tag) nil tag)
          table (:classes *classes*)
          fulls (get (:by-simple *classes*) (simple-name t))]
      (or (get table t)
          (get table (get *imports* (simple-name t)))
          (when (= 1 (count fulls)) (get table (first fulls)))))))

(defn- js-aliases-of
  "Aliases and referred names from STRING requires — (:require [\"d3\" :as d3]
  [\"@cosmograph/cosmos\" :refer [Graph]]) — which shadow-cljs tags `js`:
  `d3`, `d3/scaleSequential` and `Graph` are known receivers. A symbol
  require is a cljs namespace and its vars are typed by their own tags."
  [zloc]
  (let [ns-form (some->> (when zloc (collect zloc #(= "ns" (head-name %)))) first)
        form (when ns-form (sexpr ns-form ::no))]
    (if (or (nil? form) (= ::no form) (not (seq? form)))
      #{}
      (into #{}
            (for [c (rest form) :when (and (seq? c) (= :require (first c)))
                  spec (rest c) :when (and (vector? spec) (string? (first spec)))
                  :let [opts (apply hash-map (rest spec))]
                  nm (concat (when-let [a (:as opts)] [a]) (:refer opts))]
              (str nm))))))

(def ^:dynamic *js-aliases* #{})

(defn- imports-of
  "{simple full} from an ns form's :import clauses."
  [zloc]
  (let [ns-form (some->> (when zloc (collect zloc #(= "ns" (head-name %)))) first)
        form (when ns-form (sexpr ns-form ::no))]
    (if (or (nil? form) (= ::no form) (not (seq? form)))
      {}
      (into {}
            (for [c (rest form) :when (and (seq? c) (= :import (first c)))
                  spec (rest c)
                  full (cond (symbol? spec) [(str spec)]
                             (sequential? spec) (map #(str (first spec) "." %) (rest spec))
                             :else [])]
              [(simple-name full) full])))))

(defn- concept-class
  "A tag as the class the compiler would carry for it: a concept keyword via
  hosts.edn :concept-classes, \"Number\" and \"Fn\" via the same table, a
  class simple name as itself."
  [host tag]
  (cond (keyword? tag) (get-in hosts [host :concept-classes tag])
        (string? tag) (get-in hosts [host :concept-classes tag] tag)
        :else nil))

(defn- primitive-name? [host p] (contains? (get-in hosts [host :primitives]) p))

(defn- assignable?
  "Is a value of class `arg` assignable to `param`? Same simple name, or
  param among arg's dumped supertypes; an undumped arg class is UNKNOWN and
  matches any reference parameter — the safe reading, since the compiler
  knows the class and this walker does not."
  [param arg]
  (or (= param arg)
      (= "Object" param)
      (if-let [e (class-entry arg)]
        (boolean (some #{param} (:supers e)))
        true)))

(defn- param-match?
  "Compiler.paramArgTypeMatch, over tags. An UNTYPED argument is Object to
  the compiler (getMatchingParams substitutes Object.class when
  hasJavaClass() is false) and Object fits only an Object parameter — so
  (ProcessBuilder. x) reflects, and (OutputStreamWriter. o \"UTF-8\") on an
  untyped o did on lume. A `nil` literal is the exception: null fits any
  reference parameter. A primitive argument fits its own primitive and the
  widenings the compiler allows (int -> long, float -> double) — and NOT
  its wrapper: `(double x)` against a `Double` parameter reflects, which is
  35 of clojure-mcp's 52 reflection misses, every langchain4j builder
  setter. \"host\" — a class the compiler knows and this walker does not —
  is :undecidable, and `pick` gives no verdict."
  [host param arg]
  (let [arg (concept-class host arg)
        arg (if (string? arg) (simple-name arg) arg)
        param (simple-name param)]
    (cond
      (nil? arg) (= "Object" param)
      (= "nil" arg) (not (primitive-name? host param))
      (= param arg) true
      ;; boxed arithmetic is a java.lang.Number and nothing more specific
      (= "Number" arg) (contains? #{"Number" "Object" "Serializable"} param)
      (= "host" arg) :undecidable
      (and (not (primitive-name? host arg)) (nil? (class-entry arg))) :undecidable
      (primitive-name? host param)
      (case param
        "int" (contains? #{"Integer" "long" "Long" "short" "byte"} arg)
        "long" (contains? #{"Long" "int" "short" "byte"} arg)
        "double" (contains? #{"Double" "float"} arg)
        "float" (contains? #{"Float" "double"} arg)
        "char" (= "Character" arg) "short" (= "Short" arg) "byte" (= "Byte" arg)
        "boolean" (= "Boolean" arg)
        false)
      (primitive-name? host arg) false
      :else (assignable? param arg))))

(defn- pick
  "The compiler's choice among the overloads at this arity — Compiler's
  InstanceMethodExpr / StaticMethodExpr / NewExpr. ONE method of that name
  and arity is taken without looking at the arguments at all (a cast is
  emitted): (System/getenv k) and (.setJdbcUrl cfg (:url m)) resolve on an
  untyped argument, 38 false positives on lume until this said so. Several,
  and getMatchingParams runs: none fit -> :reflect; one -> it; more -> the
  most specific, no warning. And a method whose only declarer is a
  non-public class reflects however it was matched — getAsMethodOfPublicBase
  finds nothing — which is (.maxRetries (int 3)) on a langchain4j builder
  whose base is package-private, and was read as a wrapper mismatch until
  the dump said the method was alone at its arity."
  [host overloads arg-tags]
  (let [n (count arg-tags)
        at (filter #(= n (count (:params %))) overloads)
        ret (fn [r] (cond (nil? r) "host" (contains? #{"void" "Object"} r) "host" :else r))
        verdict (fn [ms]
                  (if (some #(false? (:public? % true)) ms)
                    {:status :reflect :reason :non-public-declarer}
                    (let [rs (distinct (map :returns ms))]
                      {:status :resolved :returns (if (= 1 (count rs)) (ret (first rs)) "host")})))]
    (cond (empty? at) nil
          (= 1 (count at)) (verdict at)
          :else
          (let [verdicts (map (fn [o] (map (partial param-match? host) (:params o) arg-tags)) at)
                ok (keep (fn [[o vs]] (when (every? true? vs) o)) (map vector at verdicts))
                ;; an :undecidable argument on an overload that is otherwise a fit
                ;; means the compiler may have resolved it; no verdict
                undecidable? (some (fn [vs] (and (some #{:undecidable} vs) (not-any? false? vs))) verdicts)]
            ;; :undecided is a verdict too — "the compiler knows, this walker does
            ;; not" — and must not fall through to a no-such-arity reflect
            ;; getMatchingParams prefers an overload whose every parameter is
            ;; the argument's exact class (foundExact): abs(double) over
            ;; abs(float), which a double also fits — kora, eleven sites
            (cond (and (empty? ok) undecidable?) {:status :undecided}
                  (empty? ok) {:status :reflect}
                  :else (let [args' (map (fn [a] (simple-name (concept-class host a))) arg-tags)
                              exact (filter (fn [o] (= (map simple-name (:params o)) args')) ok)]
                          (verdict (if (seq exact) exact ok))))))))

(defn- judge-method
  "`.m` on a receiver tagged `recv` with these argument tags: {:status
  :resolved :returns R} | {:status :reflect} | nil when the class is not in
  the dump (then the compiler knows and this walker does not). A dumped
  class with NO such method or field reflects — `(.getData e)` on an
  Exception, clojure-mcp — and a public field with no arguments is a field."
  [host recv mname arg-tags]
  (when-let [e (class-entry recv)]
    (let [ms (remove :static? (get-in e [:methods mname]))
          f (get-in e [:fields mname])]
      (cond (seq ms) (or (pick host ms arg-tags)
                         (when (and f (empty? arg-tags)) {:status :resolved :returns f})
                         {:status :reflect :reason :no-such-arity})
            (and f (empty? arg-tags)) {:status :resolved :returns f}
            :else {:status :reflect :reason :no-such-method}))))

(defn- judge-static [host class mname arg-tags]
  (when-let [e (class-entry class)]
    (let [ms (filter :static? (get-in e [:methods mname]))
          f (get-in e [:fields mname])]
      ;; (Math/abs boxed) REFLECTS — kora.core, 22 of them; a rule that said a
      ;; numeric family took a boxed Number lived for one measurement and was
      ;; a position mismatch across two adjacent lines
      (cond (seq ms) (or (pick host ms arg-tags) {:status :reflect :reason :no-such-arity})
            (and f (empty? arg-tags)) {:status :resolved :returns f}
            :else {:status :reflect :reason :no-such-member}))))

(defn- judge-ctor [host class arg-tags]
  (when-let [cs (:ctors (class-entry class))]
    (some-> (pick host (map #(assoc % :returns class) cs) arg-tags))))

(defn- static-return
  "`Character/digit` -> the dump's answer for the class and method, if any."
  [host hf arg-tags]
  (when (and hf (str/includes? hf "/") (not (str/includes? hf "/.")))
    (let [[cls m] (str/split hf #"/" 2)]
      (judge-static host (simple-name cls) m arg-tags))))

(defn- method-return
  "`.getResponseCode` on a receiver tagged HttpURLConnection returns `int`
  if the dump knows the class; \"host\" (known, unnamed) if it does not; nil
  — Object — when the call reflects, which the walker reports."
  [host env c h]
  (let [recv (second (children c))]
    (when (and recv (receiver-known? host env recv))
      (let [j (judge-method host (tag-of host env recv) (subs h 1) (map #(tag-of host env %) (drop 2 (children c))))]
        (case (:status j)
          :resolved (:returns j)
          :reflect nil
          "host")))))

(defn- agreeing-tag
  "One tag when every branch carries it, else nil — (if x (.a b) b) on a
  known b is known; lume webauthn.clj rebinds a builder through if-let and
  cond three times."
  [tags]
  (when (every? known? tags)
    (let [named (distinct (remove #{"host"} tags))]
      (cond (empty? named) "host"
            (= 1 (count named)) (first named)
            ;; known on every branch, named differently: known, unnamed
            :else "host"))))

(defn- inlined?
  "Does the compiler reach Numbers for this call at this arity? See
  hosts.edn :inline-arities."
  [host h nargs]
  (let [rule (get-in hosts [host :inline-arities h])]
    (cond (nil? rule) true
          (set? rule) (contains? rule nargs)
          (= :two-or-more rule) (>= nargs 2)
          (= :one-or-more rule) (>= nargs 1)
          :else true)))

(defn- tag-of
  "The tag the compiler would carry for this form, or nil."
  [host env zloc]
  (let [c (peel zloc)]
    (or (when-not (contains? #{:vector :map :set} (z/tag c))
          ;; ^java.util.List [a b] is a MetaExpr, and a MetaExpr has no class:
          ;; the hint on a collection literal types nothing — darling-toolkit,
          ;; three (ProcessBuilder. ^List […]) the compiler still reflected on
          (hint-of zloc))
        (some->> (host-branch host c) (tag-of host env))
        (literal-tag host c)
        (when (= :token (z/tag c))
          (when-let [nm (token-name c)]
            (or (get env nm)
                (token-var-tag c nm)
                ;; Class/FIELD — a static field the compiler resolves:
                ;; StandardCharsets/UTF_8 as a constructor argument, lume.
                ;; token-name is the NAME part; the class is in the full symbol.
                ;; The dump knows the field's type; without it, known-unnamed.
                (let [full (str (sexpr c ::no))]
                  (when (re-find (re-pattern (get-in hosts [host :interop :static])) full)
                    (let [j (static-return host full [])]
                      (if (= :resolved (:status j)) (:returns j) "host")))))))
        ;; #(…) is a fn
        (when (= :fn (z/tag c)) "Fn")
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
              ;; (d3/scaleSequential) — a call through a string-require alias is js
              (and (= host :js) (head-full c) (contains? *js-aliases* (str (namespace (symbol (head-full c)))))) "host"
              ;; a static call: the dump judges it first — (Math/abs boxed) answers
              ;; a boxed Number where the hand table below says double; the
              ;; table is the text-only fallback. Undumped is known, unnamed.
              (and (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :static]))))
                   (static-return host (head-full c) (map #(tag-of host env %) args)))
              (let [j (static-return host (head-full c) (map #(tag-of host env %) args))]
                (case (:status j) :resolved (:returns j) :reflect nil "host"))
              ;; .indexOf is an int only when the call resolved; on an unknown
              ;; receiver it reflects and returns Object (agentia ledger.clj:296)
              (and (or (get-in hosts [host :host-returns (head-full c)])
                       (get-in hosts [host :host-returns h]))
                   (or (not (interop-kind host h))
                       (some->> (second (children c)) (receiver-known? host env))))
              (or (get-in hosts [host :host-returns (head-full c)])
                  (get-in hosts [host :host-returns h]))
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :static])))) "host"
              ;; (Foo. …) is a Foo — named, so an overloaded .m on it can be
              ;; judged — unless the constructor itself reflects, then Object
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :ctor]))))
              ;; NewExpr.getJavaClass is the class whether or not the constructor
              ;; resolved: (OutputStreamWriter. o "UTF-8") on an untyped o reflects
              ;; and the result is still a Writer — lume, three false positives
              (simple-name (subs (head-full c) 0 (dec (count (head-full c)))))
              (and (= "new" h) (some-> (second (children c)) token-name)) (simple-name (some-> (second (children c)) token-name))
              ;; threading: the value carried step by step — see thread-tag
              (contains? #{"doto" "->" ".." "cond->" "some->"} h) (thread-tag host env c nil nil)
              ;; a var the corpus or the host says returns a class
              (var-return-tag c) (var-return-tag c)
              (and (interop-kind host h) (method-return host env c h)) (method-return host env c h)
              (= :arith (get hosts-core (core-head c h))) (arith-tag host env h args)
              (contains? hosts-core (core-head c h)) (get hosts-core h)
              ;; (let [...] body) / (do ... body): the last form's tag
              (contains? #{"let" "let*" "do" "when" "when-not"} h)
              (let [env' (if (contains? #{"let" "let*"} h) (bind-env host env (second (children c))) env)]
                (some->> (children c) last (tag-of host env')))
              ;; branches that agree: (if t A B), (if-let [x …] A B), (cond … A … B)
              (contains? #{"if" "if-not"} h)
              (let [[_ _ a b] (children c)] (when b (agreeing-tag (map #(tag-of host env %) [a b]))))
              (contains? #{"if-let" "if-some"} h)
              (let [[_ bvec a b] (children c)
                    env' (bind-env host env (peel bvec))]
                (when b (agreeing-tag [(tag-of host env' a) (tag-of host env b)])))
              (= "cond" h)
              (let [exprs (map second (partition 2 (rest (children c))))]
                (when (seq exprs) (agreeing-tag (map #(tag-of host env %) exprs))))
              :else nil))))))

(defn- bind-env
  "Extend `env` with a binding vector's names, each tagged by its hint or
  its init's tag. `element?` — doseq and for — binds each ELEMENT of the
  init, which the compiler never types: (doseq [f (.listFiles d)] (.isFile
  f)) reflects, and the walker gave f the array's tag until clojure-mcp.
  `:let [x …]` inside those vectors binds as let does."
  ([host env bvec] (bind-env host env bvec false))
  ([host env bvec element?]
   (if (and bvec (z/vector? bvec))
     (reduce (fn [e [lhs rhs]]
               (let [k (sexpr (peel lhs) ::no)]
                 (cond
                   (= :let k) (bind-env host e (peel rhs) false)
                   (keyword? k) e
                   (token-name lhs) (assoc e (token-name lhs) (or (hint-of lhs) (when-not element? (tag-of host e rhs))))
                   ;; a destructuring form: each symbol by its own hint, nothing else
                   :else (reduce (fn [e [nm t]] (assoc e nm t)) e (destructured lhs)))))
             env (vec-pairs bvec))
     env)))

(defn- thread-tag
  "The tag carried through (-> x s1 s2 …) / (.. x a b) / (cond-> x t s …) /
  (some-> x s …) / (doto x …), judging each step on the value threaded into
  it and, when `emit!` is given, reporting the steps that reflect. What the
  compiler does, step by step: a member step is (.m g args) and resolves on
  g's class; a static or constructor step takes g as its FIRST argument; a
  `^Hint` on a step is the tag after it; a nested threading step starts
  from g; a bare core fn step is (f g) — tagged only if the var is (`str`,
  `io/file`); anything else is an invoke and returns Object. After a
  reflective method the value is Object and every later step reflects too
  — clojure-mcp's builder chains. A constructor keeps its class whether or
  not it resolved. doto returns its target."
  [host env c emit! start-override]
  (let [h (head* c)
        kids (children c)
        ;; a nested threading STEP has no target of its own: (-> b (cond-> t (.m)))
        ;; threads b in, and its first form is a test, not a value
        nested? (some? start-override)
        target (when-not nested? (second kids))
        cond? (str/starts-with? h "cond")
        ;; #_(.logging) in a chain is not a step — clojure-mcp core.clj:225
        body (remove #(= :uneval (z/tag (peel %))) (if nested? (rest kids) (drop 2 kids)))
        steps (if cond? (map second (partition 2 body)) body)
        math (get-in hosts [host :math])
        start (if nested? start-override (tag-of host env target))
        static-re (re-pattern (get-in hosts [host :interop :static]))
        ctor-re (re-pattern (get-in hosts [host :interop :ctor]))
        emit (fn [& a] (when emit! (apply emit! a)))
        judge (fn [cur st-raw]
                ;; one step on the threaded value `cur`: the tag after it
                (let [hint (hint-of st-raw)
                      st (peel st-raw)
                      list? (or (z/list? st) (= :fn (z/tag st)))
                      nested-thread? (and list? (contains? #{"->" ".." "cond->" "some->" "doto"} (head* st)))
                      tok (token-name st)
                      ;; (.. x (a 1) b): steps have no dot
                      sh (cond (= ".." h) (some->> (or tok (head* st)) (str "."))
                               list? (head* st)
                               :else tok)
                      hf (when list? (head-full c))
                      ik (interop-kind host sh)
                      at (if tok c st)                 ; a bare step has no meta; the compiler reports the thread form
                      args (if list? (rest (children st)) [])
                      ;; a nested threading step walks its own parts; every other list
                      ;; step's arguments are code
                      _ (when (and list? (not nested-thread?)) (emit ::walk-args st nil))
                      arg-tags (map #(tag-of host env %) args)
                      full (when list? (head-full st))
                      recv (some-> target peel z/string)
                      tag (cond
                            (nil? sh) nil
                            ;; math: (cond-> n t inc) / (-> n (+ 1))
                            (contains? math (core-head st sh))
                            (let [tags (cons cur arg-tags) prim? (every? #(primitive? host %) tags)]
                              (when-not prim? (emit :boxed-math at {:op sh :tags (vec tags)}))
                              (cond (some #{"double" "float"} tags) "double" (not prim?) "Number" (= "/" sh) "Number" :else "long"))
                            ;; a member step
                            ik
                            (if-not (known? cur)
                              (do (when-not (and (= host :js) *externs* (contains? *externs* (subs sh (if (= :field ik) 2 1))))
                                    (emit (if (= host :js) :uninferred :reflection) at {:op sh :interop ik :receiver recv}))
                                  nil)
                              (let [j (when (= :instance-call ik) (judge-method host cur (subs sh 1) arg-tags))]
                                (case (:status j)
                                  :reflect (do (emit :reflection at {:op sh :interop :overload :receiver recv :tags (vec arg-tags)}) nil)
                                  :resolved (:returns j)
                                  "host")))
                            ;; a nested threading step starts from the threaded value, and
                            ;; its tests are code too
                            nested-thread?
                            (do (when (str/starts-with? sh "cond")
                                  (doseq [t (map first (partition 2 (rest (children st))))] (emit ::walk t nil)))
                                (thread-tag host env st emit! (or cur ::nil)))
                            ;; a static call with g as its first argument
                            (and full (re-find static-re full))
                            (let [j (static-return host full (cons cur arg-tags))]
                              (case (:status j)
                                :reflect (do (emit :reflection st {:op sh :interop :overload :tags (vec (cons cur arg-tags))}) nil)
                                :resolved (:returns j)
                                "host"))
                            ;; a constructor with g as its first argument: its class, resolved or not
                            (and full (re-find ctor-re full))
                            (let [cls (simple-name (subs full 0 (dec (count full))))
                                  j (judge-ctor host cls (cons cur arg-tags))]
                              (when (= :reflect (:status j)) (emit :reflection st {:op sh :interop :overload :tags (vec (cons cur arg-tags))}))
                              cls)
                            ;; a core fn the compiler knows the return of; any other invoke is Object
                            :else (let [t (or (get-in hosts [host :core (core-head st sh)]) (var-return-tag st))]
                                    (when (string? t) t)))]
                  (or hint (if (= "doto" h) cur tag))))]
    (reduce (fn [cur st] (let [cur (if (= ::nil cur) nil cur)] (judge cur st)))
            (if (= ::nil start) nil start) steps)))

;; ---- predictions ---------------------------------------------------------

(defn- interop-kind [host h]
  (when h
    (let [{:keys [instance-call field]} (get-in hosts [host :interop])]
      (cond (re-find (re-pattern field) h) :field
            (re-find (re-pattern instance-call) h) :instance-call
            :else nil))))

(defn- receiver-known? [host env zloc]
  (let [t (tag-of host env zloc)
        nm (token-name (peel zloc))
        full (some-> (peel zloc) (sexpr ::no) (#(when (symbol? %) (str %))))]
    (or (known? t)
        (and nm (str/starts-with? nm "js/"))
        ;; d3 / d3/scaleSequential / Graph from a string require
        (and full (= host :js) (or (contains? *js-aliases* full)
                                   (contains? *js-aliases* (namespace (symbol full)))))
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
    (letfn [(walk-binds [env bvec element?]
              ;; inits and :when/:let/:while forms are code too, and the
              ;; oracle's first false negatives were exactly there
              (when (and bvec (z/vector? (peel bvec)))
                (reduce (fn [e [lhs rhs]]
                          (let [k (sexpr (peel lhs) ::no)]
                            (cond
                              (= :let k) (or (walk-binds e rhs false) e)
                              (keyword? k) (do (walk e rhs) e)
                              :else (do (walk e rhs)
                                        (if-let [nm (token-name lhs)]
                                          (assoc e nm (or (hint-of lhs) (when-not element? (tag-of host e rhs))))
                                          (reduce (fn [e [nm t]] (assoc e nm t)) e (destructured lhs)))))))
                        env (vec-pairs (peel bvec)))))
            (walk [env c]
              (let [c (peel c)]
                (cond
                  (nil? c) nil
                  ;; a vector, map or set literal holds code — the first
                  ;; misses were math inside a map literal's values
                  ;; @(d/transact conn [[… (+ now ttl)]]) — a :deref is a node
                  ;; too, and three of agentia's four boxed misses were under one
                  (and (= :reader-macro (z/tag c)) (host-branch host c))
                  (walk env (host-branch host c))
                  (contains? #{:vector :map :set :deref :reader-macro :namespaced-map} (z/tag c))
                  (doseq [k (children c)] (walk env k))
                  (or (z/list? c) (= :fn (z/tag c)))
                  (let [h (head* c)
                        kids (children c)]
                    (cond
                      (contains? #{"let" "let*" "loop" "doseq" "for" "dotimes" "with-open"
                                   "if-some" "when-some" "when-first"} h)
                      (let [env' (or (walk-binds env (second kids) (contains? #{"doseq" "for" "when-first"} h)) env)]
                        ;; with-open expands to (.close x) for each binding, at the
                        ;; form's own position — clojure-mcp nrepl.clj:221
                        (when (and (= "with-open" h) (= host :jvm) (z/vector? (peel (second kids))))
                          (doseq [[lhs _] (vec-pairs (peel (second kids)))
                                  :let [nm (token-name lhs)]
                                  :when (and nm (not (known? (get env' nm))))]
                            (emit! :reflection c {:op ".close" :interop :instance-call :receiver nm})))
                        (doseq [k (drop 2 kids)] (walk env' k)))

                      ;; (doto x (.a) (.b)) / (-> x (.a) (.b)) / (.. x a b): the
                      ;; receiver of each member form is the threaded value,
                      ;; not its first argument. Measured on lume: every
                      ;; (doto (HikariConfig.) (.setJdbcUrl …)) was a false
                      ;; positive until this.
                      ;; (cond-> depth branch? inc): a bare math step is a call on
                      ;; the threaded value — sift parse.cljc:85, the one miss left
                      ;; threading macros: one resolver, see thread-tag. ->> threads
                      ;; into the LAST position, so its steps are plain calls.
                      (contains? #{"doto" "->" ".." "cond->" "some->"} h)
                      (let [kids kids]
                        (walk env (second kids))
                        (when (str/starts-with? h "cond")
                          (doseq [t (map first (partition 2 (drop 2 kids)))] (walk env t)))
                        (thread-tag host env c
                                    (fn [kind st detail]
                                      (case kind
                                        ;; every list step's arguments are code, judged or not
                                        ::walk-args (doseq [k (rest (children (peel st)))] (walk env k))
                                        ::walk (walk env st)
                                        (emit! kind st detail)))
                                    nil))

                      ;; reify / proxy / deftype / defrecord: a method's parameters
                      ;; are typed by the interface — (execute [_ request _]) on a
                      ;; ToolExecutor has a ToolExecutionRequest, and the compiler
                      ;; resolves (.name request) where a text pass saw Object.
                      (contains? #{"reify" "proxy" "deftype" "defrecord"} h)
                      (let [ifaces (->> kids (map peel) (filter #(and (= :token (z/tag %)) (token-name %))) (map token-name)
                                        (concat (when (= "proxy" h) (some->> (second kids) peel children (map token-name))))
                                        (remove nil?))
                            methods (filter #(and (z/list? (peel %)) (some-> (peel %) children second peel z/vector?)) kids)]
                        (doseq [k kids :when (not (some #{k} methods))] (walk env k))
                        (doseq [m methods
                                :let [m (peel m)
                                      mname (head* m)
                                      argv (peel (second (children m)))
                                      params (vec (children argv))
                                      ;; reify/deftype/defrecord methods take `this` first; proxy does not
                                      this? (not= "proxy" h)
                                      real (if this? (rest params) params)
                                      sig (some (fn [i] (some->> (get-in (class-entry i) [:methods mname])
                                                                 (filter #(= (count real) (count (:params %))))
                                                                 seq))
                                                ifaces)
                                      typed (when (and sig (= 1 (count sig))) (:params (first sig)))
                                      env' (reduce (fn [e [p t]] (if-let [nm (token-name p)] (assoc e nm (or (hint-of p) t)) e))
                                                   env (map vector real (or typed (repeat nil))))]]
                          (doseq [k (drop 2 (children m))] (walk env' k))))

                      (contains? #{"fn" "fn*" "defn" "defn-" "defmethod"} h)
                      (let [argv (first (filter #(z/vector? (peel %)) kids))
                            ;; (defn f ([^double t] …) ([^double t ps] …)) — each arity
                            ;; carries its own hints; the first VECTOR child is none of
                            ;; them, and kora's calculus read as 553 boxed operations
                            arities (when-not argv
                                      (filter #(and (z/list? (peel %)) (some-> (peel %) children first peel z/vector?)) kids))]
                        (if (seq arities)
                          (doseq [k kids]
                            (if (some #{k} arities)
                              (let [a (peel k) av (peel (first (children a)))
                                    env' (merge env (param-env av))]
                                (doseq [b (rest (children a))] (walk env' b)))
                              (walk env k)))
                          (let [env' (if argv (merge env (param-env (peel argv))) env)]
                            (doseq [k kids :when (not= k argv)] (walk env' k)))))

                      ;; NOT occurrence typing. (when (and (string? s) (.startsWith s
                      ;; "$")) …) reflects — kora.core tokens.cljc, three of them —
                      ;; because the Clojure compiler never narrows a local on a
                      ;; predicate, `instance?` included; that was Typed Racket's rule
                      ;; and it moved lume by one finding that had a hint elsewhere.
                      ;; Retracted; hosts.edn :narrows stays as the record of it.
                      (contains? #{"if" "when" "if-let" "when-let" "and" "cond"} h)
                      (let [narrow (fn [env _test] env)]
                        (case h
                          ("if" "when")
                          (let [[_ test & body] kids]
                            (walk env test)
                            (let [env' (narrow env test)]
                              (walk env' (first body))
                              (doseq [k (rest body)] (walk (if (= h "when") env' env) k))))
                          "and"
                          (reduce (fn [e k] (walk e k) (narrow e k)) env (rest kids))
                          "cond"
                          (loop [e env pairs (partition-all 2 (rest kids))]
                            (when-let [[test expr] (first pairs)]
                              (walk e test)
                              (when expr (walk (narrow e test) expr))
                              (recur e (rest pairs))))
                          ;; if-let / when-let: binding, then the body under the binding
                          (let [env' (or (walk-binds env (second kids) false) env)]
                            (doseq [k (drop 2 kids)] (walk env' k)))))

                      (contains? #{"catch"} h)
                      (let [[_ cls b & body] kids
                            cls (or (host-branch host cls) cls)
                            env' (if-let [nm (token-name b)] (assoc env nm (some-> cls token-name)) env)]
                        (doseq [k body] (walk env' k)))

                      :else
                      (do
                        (when (and h (contains? math (core-head c h)) (inlined? host h (count (rest kids))))
                          (let [tags (map #(tag-of host env %) (rest kids))]
                            (when-not (every? #(primitive? host %) tags)
                              (emit! :boxed-math c {:op h :tags (vec tags)}))))
                        ;; (.-k o) on an object #js built here: the extern trap.
                        ;; A rule, not a compiler prediction, so it emits
                        ;; whatever :warns says; counterpart (aget o "k").
                        (when (and (= :field (interop-kind host h)) (second kids)
                                   (= "js-literal" (tag-of host env (second kids))))
                          (let [[line col] (or (pos-of c) [nil nil])
                                nm (some-> (second kids) peel z/string)]
                            (vswap! out conj {:kind :js-prop-on-own-object :line line :column col
                                              :op h :receiver nm :prop (subs h 2)
                                              :counterpart (list 'aget (symbol nm) (subs h 2))})))
                        (when-let [ik (interop-kind host h)]
                          (when-let [recv (second kids)]
                            (if-not (receiver-known? host env recv)
                              (when-not (and (= host :js) *externs* (contains? *externs* (subs h (if (= :field ik) 2 1))))
                                (emit! (if (= host :js) :uninferred :reflection) c
                                       {:op h :interop ik :receiver (some-> recv peel z/string)}))
                              ;; known receiver: the dump judges the overload
                              (when (and (= host :jvm) (= ik :instance-call))
                                (let [tags (map #(tag-of host env %) (drop 2 kids))
                                      j (judge-method host (tag-of host env recv) (subs h 1) tags)]
                                  (when (= :reflect (:status j))
                                    (emit! :reflection c {:op h :interop :overload :receiver (some-> recv peel z/string) :tags (vec tags)})))))))
                        ;; a constructor or static call resolves an overload by
                        ;; the compiler's own parameter matching, from the dump.
                        ;; Without a dump, the hand table in hosts.edn :overloaded
                        ;; and an untyped argument is the text-only guess.
                        (when (= host :jvm)
                          (let [hf (head-full c)
                                tags (map #(tag-of host env %) (rest kids))
                                ctor? (and hf (re-find (re-pattern (get-in hosts [host :interop :ctor])) hf))
                                static? (and hf (not ctor?) (re-find (re-pattern (get-in hosts [host :interop :static])) hf))
                                j (cond ctor? (judge-ctor host (simple-name (subs hf 0 (dec (count hf)))) tags)
                                        static? (static-return host hf tags)
                                        :else nil)]
                            (cond
                              (= :reflect (:status j))
                              (emit! :reflection c {:op h :interop :overload :tags (vec tags)})
                              (and (nil? j) ctor? (nil? *classes*)
                                   (contains? (get-in hosts [host :overloaded]) hf)
                                   (some #(or (nil? %) (= "Number" %)) tags))
                              (emit! :reflection c {:op h :interop :overload :tags (vec tags)}))))
                        (doseq [k kids] (walk env k))))))))]
      (walk env zloc)
      @out)))

(defn- defn-forms [zloc]
  (collect zloc (fn [c] (contains? #{"defn" "defn-" "defmethod" "defmacro"} (head-name c)))))

(defn- ns-name-of [zloc]
  (some->> (when zloc (collect zloc #(= "ns" (head-name %)))) first children second token-name))

(defn- const-def?
  "(def ^:const x …) — the compiler inlines the value at every use, so the
  value's literal tag is the var's."
  [nm]
  (and (= :meta (z/tag nm))
       (let [form (sexpr (first (children nm)) ::no)]
         (or (= :const form) (and (map? form) (:const form))))))

(defn- def-tags
  "{\"ns/name\" tag} for `def`s whose name carries ^Tag, or ^:const with a
  numeric, string or boolean literal. lume's trace.clj: (def ^:const
  max-record-bytes 4096), compared against a count — long against int."
  [host zloc ns-name]
  (into {}
        (for [d (collect zloc #(= "def" (head-name %)))
              :let [[_ nm init] (children d)]
              :when (and nm init (token-name nm))
              :let [t (or (hint-of nm)
                          (when (const-def? nm)
                            (let [lt (literal-tag host init)]
                              (when (string? lt) lt))))]
              :when t]
          [(str ns-name "/" (token-name nm)) t])))

(defn var-tags
  "{\"ns/name\" tag} for every defn in `text` whose name or first arglist
  carries a ^Tag — the corpus's own return hints, to merge with
  bin/var-tags' for libraries."
  ([text] (var-tags text :jvm))
  ([text host]
   (let [zloc (try (z/up (z/of-string text {:track-position? true}))
                   (catch #?(:clj Exception :cljs :default) _ nil))
         ns-name (ns-name-of zloc)]
     (if-not (and zloc ns-name)
       {}
       (into (def-tags host zloc ns-name)
             (for [d (defn-forms zloc)
                   :let [[_ nm & rest] (children d)
                         argv (first (filter #(z/vector? (peel %)) rest))
                         t (or (hint-of nm) (some-> argv hint-of))]
                   :when (and (token-name nm) t)]
               [(str ns-name "/" (token-name nm)) t]))))))

(defn predictions
  "Source -> [{:kind :line :column :op :tags/:receiver :in \"name\"} …] for
  the host `path` compiles for. `opts`: {:var-tags {} :resolution idx} —
  see `*var-tags*` and `*resolve*`."
  ([text path] (predictions text path (host-of path)))
  ([text path host] (predictions text path host nil))
  ([text path host {:keys [var-tags classes externs resolution]}]
   (binding [*var-tags* var-tags
             *classes* classes
             *externs* externs
             *resolve* (when resolution
                         (some (fn [[k v]] (when (or (str/ends-with? (str path) k) (str/ends-with? k (str path))) v)) resolution))]
     (predictions* text path host))))

(defn- reflection-unwarned
  "A JVM file with interop calls and no (set! *warn-on-reflection* true):
  every reflective call in it is silent. File-level, one finding."
  [host zloc path]
  (when (= :jvm host)
    (let [interop (collect zloc (fn [c] (let [f (head-full c)]
                                          (and f (or (interop-kind host (head* c))
                                                     (re-find (re-pattern (get-in hosts [host :interop :static])) f)
                                                     (re-find (re-pattern (get-in hosts [host :interop :ctor])) f))))))
          warned? (some (fn [c] (and (= "set!" (head-name c))
                                     (= "*warn-on-reflection*" (some-> (children c) second peel z/string))))
                        (collect zloc #(= "set!" (head-name %))))]
      (when (and (seq interop) (not warned?))
        (let [[line col] (or (pos-of (first interop)) [1 1])]
          [{:kind :reflection-unwarned :line line :column col :file path
            :count (count interop)
            :counterpart '(set! *warn-on-reflection* true)}])))))

(defn- predictions*
  [text path host]
  (let [zloc (try (z/of-string text {:track-position? true})
                  (catch #?(:clj Exception :cljs :default) _ nil))
        zloc (when zloc (z/up zloc))]
    (if-not zloc
      []
      (binding [*ns-name* (ns-name-of zloc)
                *imports* (imports-of zloc)
                *js-aliases* (js-aliases-of zloc)]
       ;; EVERY top-level form: the compiler compiles (register-converter :k
       ;; (fn [bpm] (/ 60000 bpm))) as surely as a defn, and kora's ten misses
       ;; were all inside one. :in is the def's name where there is one, else
       ;; the form's head.
       (-> (vec (for [d (->> (children zloc) (map peel) (filter #(and % (z/list? %) (not (contains? #{"ns" "comment"} (head-name %))))))
                     :let [h (head-name d)
                           nm (if (and h (str/starts-with? h "def")) (some-> (children d) second token-name) h)]
                     p (predictions-in host {} d)]
                 (assoc p :in nm :file path)))
          (into (reflection-unwarned host zloc path)))))))

(def rules
  "The two host rules that are tag questions live here beside the compiler
  predictions — they need the same env. catch-all-swallow and
  mutable-escape are shape questions and stay in host.cljc."
  {:js-prop-on-own-object {:category :warning
                           :instruction "Read your own #js object with (aget obj \"k\"): #js writes a quoted key and .-k a renamable one, and :advanced renames one side."}
   :reflection-unwarned   {:category :warning
                           :instruction "Add (set! *warn-on-reflection* true) after the ns form so the compiler reports each reflective interop call."}})

(defn findings
  "Predictions as findings, one rule per kind under :typeflow/, plus the two
  host rules above under their own ids. `opts` as for `predictions`:
  {:var-tags {} :resolution idx}."
  ([text path] (findings text path nil))
  ([text path opts]
   (for [{:keys [kind line column op tags receiver in prop counterpart count]} (predictions text path (host-of path) opts)]
     (case kind
       :js-prop-on-own-object
       {:rule :js-prop-on-own-object :family :typeflow :line line :column column
        :symbol (some-> receiver symbol) :shape :js-prop-on-own-object
        :message (str ".-" prop " on " receiver ", which #js built here: :advanced renames one side")
        :applicability :machine-applicable :counterpart counterpart
        :category :warning :instruction (get-in rules [:js-prop-on-own-object :instruction])}
       :reflection-unwarned
       {:rule :reflection-unwarned :family :typeflow :line line :column column
        :symbol '*warn-on-reflection* :shape :reflection-unwarned
        :message (str count " interop calls and no (set! *warn-on-reflection* true); reflective ones are silent")
        :applicability :unspecified :counterpart counterpart
        :category :warning :instruction (get-in rules [:reflection-unwarned :instruction])}
       {:rule (keyword "typeflow" (name kind)) :family :typeflow
        :line line :column column
        :symbol (some-> in symbol)
        :message (case kind
                   :boxed-math (str op " over " (str/join ", " (map #(or % "Object") tags)) " boxes; hint or cast the operands")
                   :reflection (str op " on " receiver " reflects; its tag is not known here")
                   :uninferred (str op " on " receiver ": Closure cannot infer the target; hint ^js or use a js/ global"))
        :applicability :unspecified}))))
