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
  `pattern/match` per form and one form yields one finding. Positions come from the zipper
  node the form was read from; the match itself is over the sexpr."
  (:require [com.typemark.sift.pattern :as pat]
            [com.typemark.sift.zip :refer [collect inside-defn? pos-of sexpr]]
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

(defn- guard-ok? [spec v]
  (cond
    (set? spec)     (contains? spec v)
    (keyword? spec) (if-let [f (get guards spec)] (boolean (f v)) false)
    :else true))

(defn- guards-ok? [{:keys [when]} binds]
  (every? (fn [[v spec]] (guard-ok? spec (get binds v))) when))

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

(defn- try-rules
  "The FIRST rule that matches wins — one form, one finding. rules.edn is
  ordered specific before general for that reason."
  [file zloc form]
  (some (fn [rule]
          (when-let [binds (pat/match (:match rule) form)]
            (when (guards-ok? rule binds)
              [(finding file zloc rule binds)])))
        rules))

(defn findings
  "Every data-rule finding under `zloc`, in document order."
  [file zloc]
  (vec (mapcat (fn [c]
                 (when (inside-defn? c)
                   (let [form (sexpr c ::no)]
                     (when (not= ::no form)
                       (try-rules file c form)))))
               (collect zloc (fn [c] (z/list? c))))))
