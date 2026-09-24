(ns net.typemark.sift.data
  "Rules as data: the engine that runs rules.edn.

  Vale's shape, not HLint's alone: a small set of rule KINDS, each
  configured by data, rather than one pattern language expected to say
  everything. Two kinds today — :rewrite (a pattern and a template, the
  counterpart is the instantiated template) and :forbid (a pattern, and a
  finding without a form). A rule that needs the zipper, positions across
  forms, or a count is a coded rule in its own namespace and is registered
  beside these; the format does not decide what is a rule, the corpus does.

  Every code list under a defining form is tried against the rules in
  order and the first match wins, so a rule costs at most one
  `pattern/match` per form and one form yields one finding.

  Semgrep's three combinators, because a pattern alone cannot say WHERE:
    :either [P …]   the form matches any of these (in place of :match)
    :not    [P …]   and none of these
    :inside P       and some ANCESTOR form matches P — with the same
                    bindings, so `(deref ?a)` :inside `(swap! ?a ?&_)` is
                    the atom read inside its own swap.
    :not-inside [P …] and NO ancestor matches any of these — an io/reader
                    that is not under a with-open.

  And two things a pattern cannot say at all:

    :scope          :body (the default, and what every rule was until
                    2026-09-10) looks only inside a defining form, because a
                    top-level `(let …)` is data being built. :top is a
                    top-level form — a `load`, a `(defprotocol X)`, a
                    `(defmulti ^:private …)` — which no data rule could see;
                    :any is anywhere. Measured: ten of the smells in the
                    nufuturo-ufcg Clojure catalogue are top-level forms and
                    the gate hid every one.
    :head-ns <class>  the head's NAMESPACE, member wild — clojure.lang.RT/*
                    is the whole of Clojure's internals and enumerating its
                    members is not a rule.

  Heads resolve through the file's own `ns` aliases before matching, as they
  already did through its `:import`, so `(a/<!! c)` is
  `clojure.core.async/<!!` and a rule names the var once, in full.

  Positions come from the zipper node the form was read from; the match
  itself is over the sexpr."
  (:refer-clojure :exclude [ns-aliases])
  (:require [clojure.string :as str]
            [net.typemark.sift.typeflow :as typeflow] [net.typemark.sift.pattern :as pat]
            [net.typemark.sift.zip :refer [children collect inside-defn? peel pos-of sexpr]]
            #?(:clj  [net.typemark.sift.data-rules :refer [load-rules]]
               :cljs [net.typemark.sift.data-rules :refer-macros [load-rules]])
            [rewrite-clj.zip :as z]))

(def rules
  "rules.edn, inlined at compile time."
  (load-rules))

(def guards
  "Named predicates a rule's :when may use. A keyword names one of these; a
  set is membership. :all-equal and :each-* apply to a variable bound by
  `...` to a vector."
  {:symbol?       symbol?
   :keyword?      keyword?
   :string?       string?
   :number?       number?
   :literal?      (fn [v] (or (keyword? v) (number? v) (string? v) (char? v) (nil? v) (boolean? v)))
   :case-key?     (fn [v] (or (keyword? v) (number? v) (string? v) (char? v)))
   :all-equal     (fn [v] (and (vector? v) (apply = v)))
   :each-case-key (fn [v] (and (vector? v) (every? #(or (keyword? %) (number? %) (string? %) (char? %)) v)))
   :each-symbol   (fn [v] (and (vector? v) (every? symbol? v)))
   :at-least-2    (fn [v] (and (vector? v) (>= (count v) 2)))
   ;; metadata survives rewrite-clj's sexpr, so ^:private is askable
   :private-meta  (fn [v] (boolean (:private (meta v))))})

(def ^:dynamic *tags*
  "{[line col] tag} from typeflow/tags for the file being scanned, so a
  guard can ask what a bound form IS, not only what it looks like."
  nil)

(def ^:dynamic *classes* "bin/oracle's table, for :tag guards' supertypes." nil)

(def ^:dynamic *imports* "{simple full} from the file's ns :import." {})

(def ^:dynamic *aliases*
  "{\"a\" \"clojure.core.async\"} from the file's ns :require, so a rule names
  a var in full once and matches it however the file spells it."
  {})

(defn ns-aliases
  "The file's own :require aliases. Public because `findings` binds it and
  a caller with a zipper may want the same map."
  [root]
  (let [nsf (first (filter #(and (seq? %) (= 'ns (first %)))
                           (map #(sexpr % ::no) (children root))))]
    (into {}
          (for [clause (rest nsf)
                :when (and (seq? clause) (#{:require :require-macros} (first clause)))
                spec (rest clause)
                :when (vector? spec)
                :let [[lib & opts] spec
                      as (second (drop-while #(not= :as %) opts))]
                :when (and as (symbol? lib))]
            [(name as) (name lib)]))))

(defn- guard-ok? [spec v pos]
  (cond
    (set? spec)     (contains? spec v)
    (keyword? spec) (case spec
                      ;; the bound form is computed — not a literal, not quoted
                      :dynamic (not (or (keyword? v) (number? v) (string? v) (char? v) (nil? v) (boolean? v)
                                        (and (seq? v) (= 'quote (first v)))))
                      (if-let [f (get guards spec)] (boolean (f v)) false))
    ;; {:tag "javax.naming.Context"} — the bound form's tag, by position, is
    ;; that class or a subtype of it in the oracle's table. Without typeflow's
    ;; tags or the table the guard is false: a rule that needs a type and has
    ;; none says nothing rather than guessing
    (map? spec)     (if-let [want (:tag spec)]
                      (let [t (get *tags* pos)]
                        (and (some? t) (typeflow/assignable-to? *classes* t want)))
                      true)
    :else true))

(defn- guards-ok?
  "A guard on a variable this alternative did not bind is not a guard on
  this match — (.lookup ?ctx ?n) binds ?ctx, (InitialContext/doLookup ?n)
  does not, and one :when serves both."
  [{:keys [when]} binds positions]
  (every? (fn [[v spec]] (or (not (contains? binds v)) (guard-ok? spec (get binds v) (get positions v)))) when))

(defn- bound-positions
  "Which direct child of the matched form each variable bound — by sexpr
  equality, first match — so a :tag guard can look its position up."
  [zloc binds]
  (let [kids (for [k (children zloc) :let [p (peel k)] :when p] [(sexpr p ::no) (pos-of p)])]
    (into {} (for [[v form] binds
                   :let [pos (some (fn [[f pos]] (when (= f form) pos)) kids)]
                   :when pos]
               [v pos]))))

(defn- resolve-head
  "A form whose head is Class/member or Class. with the class spelled by
  its import — (InitialContext/doLookup n) under (:import [javax.naming
  InitialContext]) — read as the full name, so a rule names the class
  once, in full. Same for a ctor. Everything else unchanged."
  [form]
  (if (and (seq? form) (symbol? (first form)))
    (let [h (first form) ns' (namespace h) nm (name h)]
      (cond
        ;; a Clojure alias: (a/<!! c) under (:require [clojure.core.async :as a])
        (and ns' (get *aliases* ns')) (cons (symbol (get *aliases* ns') nm) (rest form))
        (and ns' (get *imports* ns')) (cons (symbol (get *imports* ns') nm) (rest form))
        ;; java.lang needs no import: (ProcessBuilder. c), (Thread/sleep n)
        (and ns' (re-matches #"[A-Z][A-Za-z0-9_$]*" ns')) (cons (symbol (str "java.lang." ns') nm) (rest form))
        (and (nil? ns') (str/ends-with? nm ".") (get *imports* (subs nm 0 (dec (count nm)))))
        (cons (symbol (str (get *imports* (subs nm 0 (dec (count nm)))) ".")) (rest form))
        (and (nil? ns') (re-matches #"[A-Z][A-Za-z0-9_$]*\." nm))
        (cons (symbol (str "java.lang." nm)) (rest form))
        :else form))
    form))

(defn- finding [file zloc {:keys [id kind emit category applicability message instruction] :as rule} binds]
  (let [[line col] (or (pos-of zloc) [nil nil])]
    (cond-> {:rule id :file file :line line :column col
             :shape id :category category :applicability applicability
             :message message :instruction instruction
             :binds binds}
      (and (= kind :rewrite) emit)
      (assoc :counterpart (pat/substitute emit
                                          ;; :all-equal vectors collapse to their one value
                                          (into {} (map (fn [[k v]]
                                                          [k (if (and (vector? v) (= :all-equal (get-in rule [:when k])))
                                                               (first v) v)]))
                                                binds))))))

(defn- unwrap-fn
  "rewrite-clj parses `#(…)` as ONE :fn node whose children are the body's
  own children — there is no list node for the body — and `sexpr` synthesises
  `(fn* [] body)` around it. So an ancestor walk skips exactly one level
  inside every #(), and `@a` in `#(swap! a assoc :k @a)` never found its own
  swap!. Yield the body forms beside the wrapper."
  [zloc form]
  (if (and (= :fn (z/tag zloc)) (seq? form) (= 'fn* (first form)))
    (cons form (filter seq? (drop 2 form)))
    [form]))

(defn- ancestors-of
  "Every enclosing FORM, outward. A `^{…}` :meta node's sexpr IS its child's,
  so without `peel` an annotated form is its own ancestor and a rule whose
  :inside pattern also matches the form itself fires on it."
  [zloc]
  (->> (iterate z/up (z/up zloc))
       (take-while some?)
       (remove #(= :meta (z/tag %)))
       (mapcat (fn [a] (let [s (sexpr a ::no)]
                         (when (not= ::no s) (unwrap-fn a s)))))
       ;; an ancestor's head resolves the same way the form's does, or
       ;; :inside (clojure.core.async/go …) never sees (a/go …)
       (map resolve-head)))

(defn- match-rule
  "Bindings for `form` under `rule`, or nil: :head-ns alone, else
  :match / :either, then :not, then :not-inside and :inside over the
  ancestors with the bindings carried through."
  [{:keys [match either not inside not-inside head-ns]} form zloc]
  (if head-ns
    (when (and (seq? form) (symbol? (first form)) (= head-ns (namespace (first form)))) {})
    (when-let [binds (if either
                       (some #(pat/match % form) either)
                       (pat/match match form))]
      (when (not-any? #(pat/match % form) not)
        (let [ancs (ancestors-of zloc)]
          (when (clojure.core/not-any? (fn [p] (some #(pat/match p % binds) ancs)) not-inside)
            (if inside
              (some (fn [p] (some (fn [a] (pat/match p a binds)) ancs))
                    (if (vector? inside) inside [inside]))
              binds)))))))

(defn- top-level?
  "A direct child of the file's root."
  [zloc]
  (let [u (z/up zloc)] (or (nil? u) (nil? (z/up u)))))

(defn- in-scope?
  "`scopes` holds the form's two answers as delays, so each is asked at most
  once per form however many rules consult it."
  [scope {:keys [body top]}]
  (case (or scope :body)
    :body @body
    :top  @top
    :any  true))

(defn- try-rules
  "The first rule that matches wins — one form, one finding. Which rule is
  first does not matter: no two rules share a pattern head, so at most one
  can match (data_rules_order_test, bin/rule-order). Each rule is asked only
  where its :scope says to look."
  [file zloc form]
  (let [form (resolve-head form)
        scopes {:body (delay (inside-defn? zloc)) :top (delay (top-level? zloc))}]
    (some (fn [rule]
            (when (in-scope? (:scope rule) scopes)
              (when-let [binds (match-rule rule form zloc)]
                (when (guards-ok? rule binds (bound-positions zloc binds))
                  [(finding file zloc rule binds)]))))
          rules)))

(defn findings
  "Every data-rule finding under `zloc`, in document order."
  [file zloc]
  ;; Any node whose sexpr is a list — a `()` list, but also `@a`, which is
  ;; a :deref node reading as (clojure.core/deref a). Collecting only
  ;; z/list? nodes made every deref invisible to every rule.
  (binding [*aliases* (ns-aliases zloc)]
    (vec (mapcat (fn [c]
                   (let [form (sexpr c ::no)]
                     (when (not= ::no form)
                       (try-rules file c form))))
                 (collect zloc (fn [c] (seq? (sexpr c ::no))))))))
