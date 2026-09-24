(ns net.typemark.sift.typeflow
  (:require [clojure.string :as str]
            [net.typemark.sift.zip :refer [children peel head-name
                                           collect pos-of sexpr token-name
                                           vec-pairs]]
            #?(:clj  [net.typemark.sift.embed :refer [load-edn]]
               :cljs [net.typemark.sift.embed :refer-macros [load-edn]])
            [rewrite-clj.zip :as z]))

(def hosts (load-edn "hosts.edn"))

(defn host-of
  [path]
  (or (when path
        (some (fn [[h {:keys [files]}]]
                (when (some #(str/ends-with? (str path) %) files) h))
              hosts))
      (some (fn [[h r]] (when (:default r) h)) hosts)
      :jvm))

(defn- head*
  [c]
  (if (= :fn (z/tag c))
    (some-> (children c) first token-name)
    (head-name c)))

(defn- head-full
  [c]
  (let [h (if (= :fn (z/tag c)) (some-> (children c) first) (some-> (children c) first))]
    (when h
      (let [s (sexpr (peel h) ::no)]
        (when (symbol? s) (str s))))))

(defn- primitive? [host tag] (contains? (get-in hosts [host :primitives]) tag))

(defn- host-branch
  [host zloc]
  (let [c (peel zloc)]
    (when (and c (= :reader-macro (z/tag c)))
      (let [[m body] (children c)]
        (when (and m (= "?" (z/string m)) body (z/list? body))
          (let [want (get-in hosts [host :feature])
                pairs (partition 2 (children body))]
            (or (some (fn [[k v]] (when (= want (sexpr k ::no)) v)) pairs)
                (some (fn [[k v]] (when (= :default (sexpr k ::no)) v)) pairs))))))))
(def ^:dynamic *tag-keywords*
  #{})

(defn- known? [tag] (some? tag))

(defn- hint-of
  [zloc]
  (when (and zloc (= :meta (z/tag zloc)))
    (let [[m] (children zloc)
          form (sexpr m ::no)]
      (cond
        (symbol? form) (name form)
        (keyword? form) (when (contains? *tag-keywords* form) (name form))
        (map? form) (some-> (or (:tag form) (get form 'tag)) str)
        :else nil))))

(defn- js-literal?
  [zloc]
  (let [c (peel zloc)]
    (or (and (= :reader-macro (z/tag c)) (= "js" (some-> (z/down c) z/string)))
        (contains? #{"clj->js" "js-obj"} (head-name c)))))

(defn- constant-coll?
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
  [argv]
  (into {}
        (for [p (children argv)
              [nm t] (destructured p)
              :when (and nm (not= "&" nm))]
          [nm t])))

(declare tag-of bind-env interop-kind receiver-known? predictions* thread-tag)

(defn- arith-tag
  [host env op args]
  (let [tags (map #(tag-of host env %) args)]
    (cond
      (and (seq tags) (some #{"double" "float"} tags)) "double"
      (and (seq tags) (every? #(primitive? host %) tags)) (if (= "/" op) "Number" "long")
      :else "Number")))

(def ^:dynamic *var-tags*
  nil)

(def ^:dynamic *resolve*
  nil)

(def ^:dynamic *ns-name*
  nil)

(defn- var-return-tag
  [c]
  (when *var-tags*
    (or (when *resolve*
          (when-let [qn (get (:vars *resolve*) (pos-of c))]
            (get *var-tags* qn)))
        (let [hf (head-full c)]
          (when hf
            (if (str/includes? hf "/")
              (get *var-tags* hf)
              (or (when *ns-name* (get *var-tags* (str *ns-name* "/" hf)))
                  (get *var-tags* (str "clojure.core/" hf)))))))))

(defn- resolved-var [c]
  (when *resolve* (get (:vars *resolve*) (pos-of c))))

(defn- core-head
  [c h]
  (when h
    (let [core? #(re-find #"^(clojure|cljs)\.core/|^clojure\.java\.io/|^io/" %)
          qn (resolved-var c)
          hf (head-full c)]
      (cond qn (when (core? qn) h)
            (= "/" hf) h
            (and hf (str/includes? hf "/") (not (core? hf))) nil
            :else h))))

(defn- token-var-tag
  [c nm]
  (when *var-tags*
    (or (some->> (resolved-var c) (get *var-tags*))
        (when *ns-name* (get *var-tags* (str *ns-name* "/" nm)))
        (when (str/includes? nm "/") (get *var-tags* nm)))))

(defn- simple-name [tag]
  (when (string? tag) (last (str/split tag #"\."))))

(def ^:dynamic *classes*
  nil)

(def ^:dynamic *externs*
  nil)

(def ^:dynamic *imports*
  {})

(defn- class-entry
  [tag]
  (when (and *classes* (string? tag))
    (let [t (if (keyword? tag) nil tag)
          table (:classes *classes*)
          fulls (get (:by-simple *classes*) (simple-name t))]
      (or (get table t)
          (get table (get *imports* (simple-name t)))
          (when (= 1 (count fulls)) (get table (first fulls)))))))

(defn- js-aliases-in
  [ns-form]
  (let [form (when ns-form (sexpr ns-form ::no))]
    (if (or (nil? form) (= ::no form) (not (seq? form)))
      #{}
      (into #{}
            (for [c (rest form) :when (and (seq? c) (= :require (first c)))
                  spec (rest c) :when (and (vector? spec) (string? (first spec)))
                  :let [opts (apply hash-map (rest spec))]
                  nm (concat (when-let [a (:as opts)] [a]) (:refer opts))]
              (str nm))))))

(def ^:dynamic *js-aliases* #{})

(defn- unknown-receiver-finding
  [host]
  (get-in hosts [host :unknown-receiver :finding]))

(defn- allowed-receiver?
  [host prop]
  (and (= :externs (get-in hosts [host :unknown-receiver :allowlist]))
       *externs* (contains? *externs* prop)))

(def ^:dynamic *annotate*
  false)

(declare predictions predictions-at inferred-at ns-env)

(defn- resolve-for
  [path resolution]
  (when resolution
    (some (fn [[k v]] (when (or (str/ends-with? (str path) k) (str/ends-with? k (str path))) v)) resolution)))

(defn- root-of
  [text]
  (try (z/up (z/of-string text)) (catch #?(:clj Exception :cljs :default) _ nil)))

(defn tags-of
  [preds]
  (into {} (for [p preds :when (= ::tag (:kind p))]
             [[(:line p) (:column p)] (:tag p)])))

(defn imports
  [text]
  (:imports (ns-env (root-of text))))

(defn assignable-to?
  [classes tag target]
  (binding [*classes* classes]
    (boolean
     (when (string? tag)
       (or (= (simple-name tag) (simple-name target))
           (some #{(simple-name target)} (:supers (class-entry tag))))))))

(defn- imports-in
  [ns-form]
  (let [form (when ns-form (sexpr ns-form ::no))]
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
  [host tag]
  (cond (keyword? tag) (get-in hosts [host :concept-classes tag])
        (string? tag) (get-in hosts [host :concept-classes tag] tag)
        :else nil))

(defn- primitive-name? [host p] (contains? (get-in hosts [host :primitives]) p))

(defn- assignable?
  [param arg]
  (or (= param arg)
      (= "Object" param)
      (if-let [e (class-entry arg)]
        (boolean (some #{param} (:supers e)))
        true)))

(defn- param-match?
  [host param arg]
  (let [arg (concept-class host arg)
        arg (if (string? arg) (simple-name arg) arg)
        param (simple-name param)]
    (cond
      (nil? arg) (= "Object" param)
      (= "nil" arg) (not (primitive-name? host param))
      (= param arg) true
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
                undecidable? (some (fn [vs] (and (some #{:undecidable} vs) (not-any? false? vs))) verdicts)]
            (cond (and (empty? ok) undecidable?) {:status :undecided}
                  (empty? ok) {:status :reflect}
                  :else (let [args' (map (fn [a] (simple-name (concept-class host a))) arg-tags)
                              exact (filter (fn [o] (= (map simple-name (:params o)) args')) ok)]
                          (verdict (if (seq exact) exact ok))))))))

(defn- judge-method
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
      (cond (seq ms) (or (pick host ms arg-tags) {:status :reflect :reason :no-such-arity})
            (and f (empty? arg-tags)) {:status :resolved :returns f}
            :else {:status :reflect :reason :no-such-member}))))

(defn- judge-ctor [host class arg-tags]
  (when-let [cs (:ctors (class-entry class))]
    (some-> (pick host (map #(assoc % :returns class) cs) arg-tags))))

(defn- static-return
  [host hf arg-tags]
  (when (and hf (str/includes? hf "/") (not (str/includes? hf "/.")))
    (let [[cls m] (str/split hf #"/" 2)]
      (judge-static host (simple-name cls) m arg-tags))))

(defn- method-return
  [host env c h]
  (let [recv (second (children c))]
    (when (and recv (receiver-known? host env recv))
      (let [j (judge-method host (tag-of host env recv) (subs h 1) (map #(tag-of host env %) (drop 2 (children c))))]
        (case (:status j)
          :resolved (:returns j)
          :reflect nil
          "host")))))

(defn- agreeing-tag
  [tags]
  (when (every? known? tags)
    (let [named (distinct (remove #{"host"} tags))]
      (cond (empty? named) "host"
            (= 1 (count named)) (first named)
            :else "host"))))

(defn- inlined?
  [host h nargs]
  (let [rule (get-in hosts [host :inline-arities h])]
    (cond (nil? rule) true
          (set? rule) (contains? rule nargs)
          (= :two-or-more rule) (>= nargs 2)
          (= :one-or-more rule) (>= nargs 1)
          :else true)))

(defn- tag-of
  [host env zloc]
  (let [c (peel zloc)]
    (or (when-not (contains? #{:vector :map :set} (z/tag c))
          (hint-of zloc))
        (some->> (host-branch host c) (tag-of host env))
        (literal-tag host c)
        (when (= :token (z/tag c))
          (when-let [nm (token-name c)]
            (or (get env nm)
                (token-var-tag c nm)
                (let [full (str (sexpr c ::no))]
                  (when (re-find (re-pattern (get-in hosts [host :interop :static])) full)
                    (let [j (static-return host full [])]
                      (if (= :resolved (:status j)) (:returns j) "host")))))))
        (when (= :fn (z/tag c)) "Fn")
        (when (or (z/list? c) (= :fn (z/tag c)))
          (let [h (head* c)
                hosts-core (get-in hosts [host :core])
                casts (get-in hosts [host :casts])
                args (rest (children c))]
            (cond
              (nil? h) nil
              (contains? casts h) (get casts h)
              (and (get-in hosts [host :string-requires]) (head-full c)
                   (contains? *js-aliases* (str (namespace (symbol (head-full c)))))) "host"
              (and (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :static]))))
                   (static-return host (head-full c) (map #(tag-of host env %) args)))
              (let [j (static-return host (head-full c) (map #(tag-of host env %) args))]
                (case (:status j) :resolved (:returns j) :reflect nil "host"))
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :static])))) "host"
              (some->> (head-full c) (re-find (re-pattern (get-in hosts [host :interop :ctor]))))
              (simple-name (subs (head-full c) 0 (dec (count (head-full c)))))
              (and (= "new" h) (some-> (second (children c)) token-name)) (simple-name (some-> (second (children c)) token-name))
              (contains? #{"doto" "->" ".." "cond->" "some->"} h) (thread-tag host env c nil nil)
              (var-return-tag c) (var-return-tag c)
              (and (interop-kind host h) (method-return host env c h)) (method-return host env c h)
              (= :arith (get hosts-core (core-head c h))) (arith-tag host env h args)
              (contains? hosts-core (core-head c h)) (get hosts-core h)
              (contains? #{"let" "let*" "do" "when" "when-not"} h)
              (let [env' (if (contains? #{"let" "let*"} h) (bind-env host env (second (children c))) env)]
                (some->> (children c) last (tag-of host env')))
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
  ([host env bvec] (bind-env host env bvec false))
  ([host env bvec element?]
   (if (and bvec (z/vector? bvec))
     (reduce (fn [e [lhs rhs]]
               (let [k (sexpr (peel lhs) ::no)]
                 (cond
                   (= :let k) (bind-env host e (peel rhs) false)
                   (keyword? k) e
                   (token-name lhs) (assoc e (token-name lhs) (or (hint-of lhs) (when-not element? (tag-of host e rhs))))
                   :else (reduce (fn [e [nm t]] (assoc e nm t)) e (destructured lhs)))))
             env (vec-pairs bvec))
     env)))

(defn- thread-tag
  [host env c emit! start-override]
  (let [h (head* c)
        kids (children c)
        nested? (some? start-override)
        target (when-not nested? (second kids))
        cond? (str/starts-with? h "cond")
        body (remove #(= :uneval (z/tag (peel %))) (if nested? (rest kids) (drop 2 kids)))
        steps (if cond? (map second (partition 2 body)) body)
        math (get-in hosts [host :math])
        start (if nested? start-override (tag-of host env target))
        static-re (re-pattern (get-in hosts [host :interop :static]))
        ctor-re (re-pattern (get-in hosts [host :interop :ctor]))
        emit (fn [& a] (when emit! (apply emit! a)))
        judge (fn [cur st-raw]
                (let [hint (hint-of st-raw)
                      st (peel st-raw)
                      list? (or (z/list? st) (= :fn (z/tag st)))
                      nested-thread? (and list? (contains? #{"->" ".." "cond->" "some->" "doto"} (head* st)))
                      tok (token-name st)
                      sh (cond (= ".." h) (some->> (or tok (head* st)) (str "."))
                               list? (head* st)
                               :else tok)
                      ik (interop-kind host sh)
                      at (if tok c st)                 args (if list? (rest (children st)) [])
                      _ (when (and list? (not nested-thread?)) (emit ::walk-args st nil))
                      arg-tags (map #(tag-of host env %) args)
                      full (when list? (head-full st))
                      recv (some-> target peel z/string)
                      tag (cond
                            (nil? sh) nil
                            (contains? math (core-head st sh))
                            (let [tags (cons cur arg-tags) prim? (every? #(primitive? host %) tags)]
                              (when-not prim? (emit :boxed-math at {:op sh :tags (vec tags)}))
                              (cond (some #{"double" "float"} tags) "double" (not prim?) "Number" (= "/" sh) "Number" :else "long"))
                            ik
                            (if-not (known? cur)
                              (do (when-not (allowed-receiver? host (subs sh (if (= :field ik) 2 1)))
                                    (emit (unknown-receiver-finding host) at {:op sh :interop ik :receiver recv}))
                                  nil)
                              (let [j (when (= :instance-call ik) (judge-method host cur (subs sh 1) arg-tags))]
                                (case (:status j)
                                  :reflect (do (emit :reflection at {:op sh :interop :overload :receiver recv :tags (vec arg-tags)}) nil)
                                  :resolved (:returns j)
                                  "host")))
                            nested-thread?
                            (do (when (str/starts-with? sh "cond")
                                  (doseq [t (map first (partition 2 (rest (children st))))] (emit ::walk t nil)))
                                (thread-tag host env st emit! (or cur ::nil)))
                            (and full (re-find static-re full))
                            (let [j (static-return host full (cons cur arg-tags))]
                              (case (:status j)
                                :reflect (do (emit :reflection st {:op sh :interop :overload :tags (vec (cons cur arg-tags))}) nil)
                                :resolved (:returns j)
                                "host"))
                            (and full (re-find ctor-re full))
                            (let [cls (simple-name (subs full 0 (dec (count full))))
                                  j (judge-ctor host cls (cons cur arg-tags))]
                              (when (= :reflect (:status j)) (emit :reflection st {:op sh :interop :overload :tags (vec (cons cur arg-tags))}))
                              cls)
                            :else (let [t (or (get-in hosts [host :core (core-head st sh)]) (var-return-tag st))]
                                    (when (string? t) t)))]
                  (or hint (if (= "doto" h) cur tag))))]
    (reduce (fn [cur st] (let [cur (if (= ::nil cur) nil cur)] (judge cur st)))
            (if (= ::nil start) nil start) steps)))

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
        (and full (get-in hosts [host :string-requires])
             (or (contains? *js-aliases* full)
                                   (contains? *js-aliases* (namespace (symbol full)))))
        (and nm (contains? (get-in hosts [host :known-receiver]) (hint-of zloc))))))

(defn- predictions-in
  [host env zloc]
  (let [out (volatile! [])
        math (get-in hosts [host :math])
        warns (get-in hosts [host :warns])
        emit! (fn [kind c detail]
                (when (or (contains? warns kind) (= ::tag kind))
                  (let [[line col] (or (pos-of c) [nil nil])
                        col (if (and col (= :fn (z/tag c))) (inc col) col)]
                    (vswap! out conj (merge {:kind kind :line line :column col} detail)))))]
    (letfn [(walk-binds [env bvec element?]
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
                (when (and *annotate* c (or (z/list? c) (token-name c)))
                  (emit! ::tag c {:tag (tag-of host env c)}))
                (cond
                  (nil? c) nil
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
                        (when-let [implied (get-in hosts [host :implicit-calls h])]
                          (when (z/vector? (peel (second kids)))
                            (doseq [[lhs _] (vec-pairs (peel (second kids)))
                                    :let [nm (token-name lhs)]
                                    :when (and nm (not (known? (get env' nm))))]
                              (emit! (unknown-receiver-finding host) c
                                     {:op implied :interop :instance-call :receiver nm}))))
                        (doseq [k (drop 2 kids)] (walk env' k)))

                      (contains? #{"doto" "->" ".." "cond->" "some->"} h)
                      (let [kids kids]
                        (walk env (second kids))
                        (when (str/starts-with? h "cond")
                          (doseq [t (map first (partition 2 (drop 2 kids)))] (walk env t)))
                        (thread-tag host env c
                                    (fn [kind st detail]
                                      (case kind
                                        ::walk-args (doseq [k (rest (children (peel st)))] (walk env k))
                                        ::walk (walk env st)
                                        (emit! kind st detail)))
                                    nil))

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
                      (let [argv (some (fn [x] (when (z/vector? (peel x)) x)) kids)
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
                        (when (and (= :field (interop-kind host h)) (second kids)
                                   (= "js-literal" (tag-of host env (second kids))))
                          (let [[line col] (or (pos-of c) [nil nil])
                                nm (some-> (second kids) peel z/string)]
                            (vswap! out conj {:kind :js-prop-on-own-object :line line :column col
                                              :op h :receiver nm :prop (subs h 2)
                                              :fix (list 'aget (symbol nm) (subs h 2))})))
                        (when-let [ik (interop-kind host h)]
                          (when-let [recv (second kids)]
                            (if-not (receiver-known? host env recv)
                              (when-not (allowed-receiver? host (subs h (if (= :field ik) 2 1)))
                                (emit! (unknown-receiver-finding host) c
                                       {:op h :interop ik :receiver (some-> recv peel z/string)}))
                              (when (and (get-in hosts [host :overloads]) (= ik :instance-call))
                                (let [tags (map #(tag-of host env %) (drop 2 kids))
                                      j (judge-method host (tag-of host env recv) (subs h 1) tags)]
                                  (when (= :reflect (:status j))
                                    (emit! :reflection c {:op h :interop :overload :receiver (some-> recv peel z/string) :tags (vec tags)})))))))
                        (when (get-in hosts [host :overloads])
                          (let [hf (head-full c)
                                tags (map #(tag-of host env %) (rest kids))
                                ctor? (and hf (re-find (re-pattern (get-in hosts [host :interop :ctor])) hf))
                                static? (and hf (not ctor?) (re-find (re-pattern (get-in hosts [host :interop :static])) hf))
                                j (cond ctor? (judge-ctor host (simple-name (subs hf 0 (dec (count hf)))) tags)
                                        static? (static-return host hf tags)
                                        :else nil)]
                            (when (= :reflect (:status j))
                              (emit! :reflection c {:op h :interop :overload :tags (vec tags)}))))
                        (doseq [k kids] (walk env k))))))))]
      (walk env zloc)
      @out)))

(defn- ns-form-of
  [zloc]
  (some->> (when zloc (collect zloc #(= "ns" (head-name %)))) first))

(defn- ns-name-in [ns-form]
  (some->> ns-form children second token-name))

(defn ns-env
  [zloc]
  (let [ns-form (ns-form-of zloc)]
    {:ns-name (ns-name-in ns-form)
     :imports (imports-in ns-form)
     :js-aliases (js-aliases-in ns-form)}))

(defn- return-tag
  [host d]
  (let [kids (children d)
        argv (some (fn [x] (when (z/vector? (peel x)) x)) kids)
        arities (if argv
                  [[(peel argv) (last kids)]]
                  (for [k kids :let [a (peel k)] :when (and (z/list? a) (some-> a children first peel z/vector?))]
                    [(peel (first (children a))) (last (children a))]))]
    (for [[av body] arities
          :let [t (tag-of host (param-env av) body)]
          :when (and (string? t) (not (contains? #{"host" "nil"} t)))]
      (simple-name t))))

(defn inferred
  ([text path] (inferred text path nil))
  ([text path opts] (inferred-at (root-of text) path opts)))

(defn inferred-at
  [zloc path {:keys [var-tags classes externs resolution] :as opts}]
  (if-not zloc
    []
    (let [host (host-of path)
          {:keys [ns-name imports js-aliases]} (or (:ns-env opts) (ns-env zloc))]
      (binding [*var-tags* var-tags *classes* classes *externs* externs
                *resolve* (resolve-for path resolution)
                *ns-name* ns-name
                *imports* imports
                *js-aliases* js-aliases]
        (vec (for [d (->> (children zloc) (map peel) (filter #(and % (z/list? %) (contains? #{"defn" "defn-"} (head-name %)))))
                   :let [[_ nm & rest] (children d)
                         argv (some (fn [x] (when (z/vector? (peel x)) x)) rest)
                         declared? (or (hint-of nm) (some-> argv hint-of))
                         [line _] (pos-of d)]
                   :when (and (token-name nm) (not declared?))
                   t (distinct (return-tag host d))]
               {:name (token-name nm) :line line :slot -1 :type t}))))))

(defn predictions
  ([text path] (predictions text path (host-of path)))
  ([text path host] (predictions text path host nil))
  ([text path host opts] (predictions-at (root-of text) path host opts)))

(defn predictions-at
  [zloc path host {:keys [var-tags classes externs resolution annotate?] :as opts}]
  (binding [*var-tags* var-tags
            *classes* classes
            *externs* externs
            *annotate* (boolean annotate?)
            *resolve* (resolve-for path resolution)]
    (predictions* zloc path host (:ns-env opts))))

(defn- predictions*
  [zloc path host env]
  (if-not zloc
    []
    (let [{:keys [ns-name imports js-aliases]} (or env (ns-env zloc))]
      (binding [*ns-name* ns-name
                *imports* imports
                *js-aliases* js-aliases
                *tag-keywords* (get-in hosts [host :tag-keywords] #{})]
        (vec (for [d (->> (children zloc) (map peel) (filter #(and % (z/list? %) (not (contains? #{"ns" "comment"} (head-name %))))))
                   :let [h (head-name d)
                         nm (if (and h (str/starts-with? h "def")) (some-> (children d) second token-name) h)]
                   p (predictions-in host {} d)]
               (assoc p :in nm :file path)))))))

(defn analysis
  [zloc path opts]
  (let [preds (predictions-at zloc path (host-of path) (assoc opts :annotate? (= :jvm (host-of path))))
        tag? #(= ::tag (:kind %))]
    {:tags (tags-of preds)
     :predictions (vec (remove tag? preds))}))

(defn hit
  [{:keys [kind line column op tags receiver in prop fix]}]
  (case kind
    :js-prop-on-own-object
    {:line line :column column :symbol (some-> receiver symbol)
     :message (str ".-" prop " on " receiver ", which #js built here: :advanced renames one side")
     :applicability :machine-applicable :fix fix}
    {:line line :column column :symbol (some-> in symbol)
     :message (case kind
                :boxed-math (str op " over " (str/join ", " (map #(or % "Object") tags)) " boxes; hint or cast the operands")
                :reflection (str op " on " receiver " reflects; its tag is not known here")
                :uninferred (str op " on " receiver ": Closure cannot infer the target; hint ^js or use a js/ global"))
     :applicability :unspecified}))
