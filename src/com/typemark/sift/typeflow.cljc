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

(defn- literal-tag [host zloc]
  (let [c (peel zloc)
        t (z/tag c)
        lit (get-in hosts [host :literal])]
    (if (js-literal? c)
      "js-literal"
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
      nil))))

(defn- param-env
  "[^String s ^long n m] -> {\"s\" \"String\" \"n\" \"long\" \"m\" nil}."
  [argv]
  (into {}
        (for [p (children argv)
              :let [nm (token-name p)]
              :when (and nm (not= "&" nm))]
          [nm (hint-of p)])))

(declare tag-of bind-env interop-kind receiver-known? defn-forms predictions*)

(defn- arith-tag [host env args]
  (let [tags (map #(tag-of host env %) args)]
    (if (and (seq tags) (every? #(primitive? host %) tags))
      (if (some #{"double" "float"} tags) "double" "long")
      "Number")))

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
    (let [core? #(re-find #"^(clojure|cljs)\.core/" %)
          qn (resolved-var c)
          hf (head-full c)]
      (cond qn (when (core? qn) h)
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

(defn- returns-of
  "A bin/var-tags entry is a tag string, or {:returns tag :overloaded #{n…}}
  for a method with several overloads; nil, \"void\" and \"Object\" are
  known-but-unnamed, which is \"host\"."
  [entry]
  (let [r (if (map? entry) (:returns entry) entry)]
    (cond (nil? r) nil
          (contains? #{"void" "Object"} r) "host"
          :else r)))

(defn- overloaded-at?
  "Does the dumped class have more than one `.m` at `nargs` arguments? Then
  an argument the compiler cannot type reflects even on a known receiver —
  lume diplomat.clj:401, (.write writer (sse-event …)) on an
  OutputStreamWriter: write(String), write(char[]), write(int)."
  [recv-tag h nargs]
  (when *var-tags*
    (let [e (some->> recv-tag simple-name (#(str % "/" h)) (get *var-tags*))]
      (and (map? e) (contains? (:overloaded e) nargs)))))

(defn- static-return
  "`Character/digit` -> the dump's \"Character/.digit\" entry, if any."
  [hf]
  (when (and *var-tags* hf (str/includes? hf "/") (not (str/includes? hf "/.")))
    (some-> (get *var-tags* (str/replace hf "/" "/.")) returns-of)))

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

(defn- method-return
  "`.getResponseCode` on a receiver tagged HttpURLConnection returns `int`,
  if bin/var-tags dumped that class — keyed \"Simple/.method\". \"host\"
  (known, unnamed) otherwise, which is what the compiler also knows when it
  resolved the call: something, but this walker cannot say what."
  [host env c h]
  (let [recv (second (children c))]
    (when (and recv (receiver-known? host env recv))
      (or (when *var-tags*
            (some->> (tag-of host env recv) simple-name (#(str % "/" h)) (get *var-tags*) returns-of))
          "host"))))

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
    (or (hint-of zloc)
        (some->> (host-branch host c) (tag-of host env))
        (literal-tag host c)
        (when (= :token (z/tag c))
          (when-let [nm (token-name c)]
            (or (get env nm)
                (token-var-tag c nm)
                ;; Class/FIELD — a static field the compiler resolves:
                ;; StandardCharsets/UTF_8 as a constructor argument, lume.
                ;; token-name is the NAME part; the class is in the full symbol
                (let [full (str (sexpr c ::no))]
                  (when (re-find (re-pattern (get-in hosts [host :interop :static])) full) "host")))))
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
              ;; .indexOf is an int only when the call resolved; on an unknown
              ;; receiver it reflects and returns Object (agentia ledger.clj:296)
              (and (or (get-in hosts [host :host-returns (head-full c)])
                       (get-in hosts [host :host-returns h]))
                   (or (not (interop-kind host h))
                       (some->> (second (children c)) (receiver-known? host env))))
              (or (get-in hosts [host :host-returns (head-full c)])
                  (get-in hosts [host :host-returns h]))
              (static-return (head-full c)) (static-return (head-full c))
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :static])))) "host"
              ;; (Foo. …) is a Foo — named, so an overloaded .m on it can be judged
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :ctor]))))
              (simple-name (subs (head-full c) 0 (dec (count (head-full c)))))
              (and (= "new" h) (some-> (second (children c)) token-name)) (simple-name (some-> (second (children c)) token-name))
              ;; (doto x …) is x; (-> x (.a) (.b)) on a known x is a chain the
              ;; compiler resolves step by step, so its result is known
              (= "doto" h) (some->> (second (children c)) (tag-of host env))
              (contains? #{"->" ".."} h) (when (some->> (second (children c)) (receiver-known? host env)) "host")
              ;; a var the corpus or the host says returns a class
              (var-return-tag c) (var-return-tag c)
              (and (interop-kind host h) (method-return host env c h)) (method-return host env c h)
              (= :arith (get hosts-core (core-head c h))) (arith-tag host env args)
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
                      (let [env' (or (walk-binds env (second kids)) env)]
                        (doseq [k (drop 2 kids)] (walk env' k)))

                      ;; (doto x (.a) (.b)) / (-> x (.a) (.b)) / (.. x a b): the
                      ;; receiver of each member form is the threaded value,
                      ;; not its first argument. Measured on lume: every
                      ;; (doto (HikariConfig.) (.setJdbcUrl …)) was a false
                      ;; positive until this.
                      ;; (cond-> depth branch? inc): a bare math step is a call on
                      ;; the threaded value — sift parse.cljc:85, the one miss left
                      (contains? #{"cond->" "cond->>" "some->" "some->>"} h)
                      (let [target (second kids)
                            steps (if (str/starts-with? h "cond") (map second (partition 2 (drop 2 kids))) (drop 2 kids))
                            tests (when (str/starts-with? h "cond") (map first (partition 2 (drop 2 kids))))]
                        (walk env target)
                        (doseq [t tests] (walk env t))
                        (reduce (fn [cur st]
                                  (let [st (peel st)
                                        tok (token-name st)
                                        sh (when (or (z/list? st) (= :fn (z/tag st))) (head* st))]
                                    (cond
                                      (and tok (contains? math (core-head st tok)))
                                      ;; a bare step becomes (inc g) with no meta of its own, so the
                                      ;; compiler reports the cond-> form's column; a list step keeps its own
                                      (do (when-not (primitive? host cur) (emit! :boxed-math c {:op tok :tags [cur]}))
                                          (arith-tag host env []))
                                      (and sh (contains? math (core-head st sh)))
                                      (let [tags (cons cur (map #(tag-of host env %) (rest (children st))))]
                                        (doseq [k (rest (children st))] (walk env k))
                                        (when-not (every? #(primitive? host %) tags)
                                          (emit! :boxed-math st {:op sh :tags (vec tags)}))
                                        (if (every? #(primitive? host %) tags) "long" "Number"))
                                      :else (do (walk env st) nil))))
                                (tag-of host env target) steps))

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

                      ;; occurrence typing: (if (string? s) THEN ELSE) — s is a
                      ;; String in THEN; (instance? C x) likewise; (and (string? s) …)
                      ;; narrows for the rest of the and. hosts.edn :narrows.
                      (contains? #{"if" "when" "if-let" "when-let" "and" "cond"} h)
                      (let [narrow (fn [env test]
                                     (let [t (peel test)]
                                       (if (and t (z/list? t))
                                         (let [th (head* t) [_ a b] (children t)]
                                           (cond
                                             (and (= "instance?" th) a b (token-name b) (token-name a))
                                             (assoc env (token-name b) (token-name a))
                                             (and a (token-name a) (get-in hosts [host :narrows th]))
                                             (assoc env (token-name a) (get-in hosts [host :narrows th]))
                                             :else env))
                                         env)))]
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
                          (let [env' (or (walk-binds env (second kids)) env)]
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
                              (emit! (if (= host :js) :uninferred :reflection) c
                                     {:op h :interop ik :receiver (some-> recv peel z/string)})
                              ;; known receiver, overloaded method, untyped argument
                              (let [args (drop 2 kids)
                                    tags (map #(tag-of host env %) args)]
                                (when (and (= host :jvm) (= ik :instance-call)
                                           (overloaded-at? (tag-of host env recv) h (count args))
                                           (some #(or (nil? %) (= "Number" %)) tags))
                                  (emit! :reflection c {:op h :interop :overload :receiver (some-> recv peel z/string) :tags (vec tags)}))))))
                        ;; a constructor or static call resolves an OVERLOAD, and
                        ;; an argument the compiler cannot type — Object, or a
                        ;; boxed Number from arithmetic — is a reflective call:
                        ;; (Date. (+ (System/currentTimeMillis) ttl)) and
                        ;; (OutputStreamWriter. stream "UTF-8") on lume, measured
                        (when (and (= host :jvm)
                                   (let [hf (head-full c)
                                         nargs (count (rest kids))
                                         dumped (when (and *var-tags* hf (re-find (re-pattern (get-in hosts [host :interop :ctor])) hf))
                                                  (get *var-tags* (str (simple-name (subs hf 0 (dec (count hf)))) "/new")))]
                                     (and hf
                                          (if dumped
                                            ;; per arity: (URL. s) has one 1-arg ctor and
                                            ;; resolves; (ProcessBuilder. x) has two — agentia
                                            (and (map? dumped) (contains? (:overloaded dumped) nargs))
                                            (contains? (get-in hosts [host :overloaded]) hf)))))
                          (let [tags (map #(tag-of host env %) (rest kids))]
                            (when (some #(or (nil? %) (= "Number" %)) tags)
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
  ([text path host {:keys [var-tags resolution]}]
   (binding [*var-tags* var-tags
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
      (binding [*ns-name* (ns-name-of zloc)]
       (-> (vec (for [d (collect zloc (fn [c] (contains? #{"defn" "defn-" "defmethod" "defmacro" "def"} (head-name c))))
                     :let [nm (some-> (children d) second token-name)]
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
