(ns com.typemark.sift.data
  "Rules as data: the engine that runs rules.edn (DEFNET-55).

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
                    the atom read inside its own swap. Positions come from the zipper
  node the form was read from; the match itself is over the sexpr."
  (:require [clojure.string :as str]
            [com.typemark.sift.typeflow :as typeflow] [com.typemark.sift.pattern :as pat]
            [com.typemark.sift.zip :refer [children collect inside-defn? peel pos-of sexpr]]
            #?(:clj  [com.typemark.sift.data-rules :refer [load-rules]]
               :cljs [com.typemark.sift.data-rules :refer-macros [load-rules]])
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
   :at-least-2    (fn [v] (and (vector? v) (>= (count v) 2)))})

(def ^:dynamic *tags*
  "{[line col] tag} from typeflow/tags for the file being scanned, so a
  guard can ask what a bound form IS, not only what it looks like."
  nil)

(def ^:dynamic *classes* "bin/oracle's table, for :tag guards' supertypes." nil)

(def ^:dynamic *imports* "{simple full} from the file's ns :import." {})

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

(defn- ancestors-of [zloc]
  (->> (iterate z/up (z/up zloc)) (take-while some?) (map #(sexpr % ::no)) (remove #{::no})))

(defn- match-rule
  "Bindings for `form` under `rule`, or nil: :match / :either, then :not,
  then :inside over the ancestors with the bindings carried through."
  [{:keys [match either not inside]} form zloc]
  (when-let [binds (if either
                     (some #(pat/match % form) either)
                     (pat/match match form))]
    (when (not-any? #(pat/match % form) not)
      (if inside
        (some (fn [anc] (pat/match inside anc binds)) (ancestors-of zloc))
        binds))))

(defn- try-rules
  "The first rule that matches wins — one form, one finding. Which rule is
  first does not matter: no two rules share a pattern head, so at most one
  can match (data_rules_order_test, bin/rule-order)."
  [file zloc form]
  (let [form (resolve-head form)]
    (some (fn [rule]
            (when-let [binds (match-rule rule form zloc)]
              (when (guards-ok? rule binds (bound-positions zloc binds))
                [(finding file zloc rule binds)])))
          rules)))

(defn findings
  "Every data-rule finding under `zloc`, in document order."
  [file zloc]
  ;; Any node whose sexpr is a list — a `()` list, but also `@a`, which is
  ;; a :deref node reading as (clojure.core/deref a). Collecting only
  ;; z/list? nodes made every deref invisible to every rule.
  (vec (mapcat (fn [c]
                 (when (inside-defn? c)
                   (let [form (sexpr c ::no)]
                     (when (not= ::no form)
                       (try-rules file c form)))))
               (collect zloc (fn [c] (seq? (sexpr c ::no)))))))
