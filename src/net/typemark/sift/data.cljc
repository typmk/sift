(ns net.typemark.sift.data
  (:refer-clojure :exclude [ns-aliases])
  (:require [clojure.string :as str]
            [net.typemark.sift.typeflow :as typeflow] [net.typemark.sift.pattern :as pat]
            [net.typemark.sift.zip :refer [children collect inside-defn? peel pos-of sexpr]]
            [rewrite-clj.zip :as z]))

(def guards
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
   :private-meta  (fn [v] (boolean (:private (meta v))))})

(defn ns-aliases
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

(defn- guard-ok? [env spec v pos]
  (cond
    (set? spec)     (contains? spec v)
    (keyword? spec) (case spec
                      :dynamic (not (or (keyword? v) (number? v) (string? v) (char? v) (nil? v) (boolean? v)
                                        (and (seq? v) (= 'quote (first v)))))
                      (if-let [f (get guards spec)] (boolean (f v)) false))
    (map? spec)     (if-let [want (:tag spec)]
                      (let [t (get (:tags env) pos)]
                        (and (some? t) (typeflow/assignable-to? (:classes env) t want)))
                      true)
    :else true))

(defn- guards-ok?
  [env {:keys [when]} binds positions]
  (every? (fn [[v spec]] (or (not (contains? binds v)) (guard-ok? env spec (get binds v) (get positions v)))) when))

(defn- bound-positions
  [zloc binds]
  (let [kids (for [k (children zloc) :let [p (peel k)] :when p] [(sexpr p ::no) (pos-of p)])]
    (into {} (for [[v form] binds
                   :let [pos (some (fn [[f pos]] (when (= f form) pos)) kids)]
                   :when pos]
               [v pos]))))

(defn- resolve-head
  [{:keys [aliases imports]} form]
  (if (and (seq? form) (symbol? (first form)))
    (let [h (first form) ns' (namespace h) nm (name h)]
      (cond
        (and ns' (get aliases ns')) (cons (symbol (get aliases ns') nm) (rest form))
        (and ns' (get imports ns')) (cons (symbol (get imports ns') nm) (rest form))
        (and ns' (re-matches #"[A-Z][A-Za-z0-9_$]*" ns')) (cons (symbol (str "java.lang." ns') nm) (rest form))
        (and (nil? ns') (str/ends-with? nm ".") (get imports (subs nm 0 (dec (count nm)))))
        (cons (symbol (str (get imports (subs nm 0 (dec (count nm)))) ".")) (rest form))
        (and (nil? ns') (re-matches #"[A-Z][A-Za-z0-9_$]*\." nm))
        (cons (symbol (str "java.lang." nm)) (rest form))
        :else form))
    form))

(defn- finding [zloc {:keys [id extends emit] :as rule} binds]
  (let [[line col] (or (pos-of zloc) [nil nil])]
    (cond-> {:rule id :line line :column col}
      (and (= extends :substitution) emit)
      (assoc :fix (pat/substitute emit
                                  (into {} (map (fn [[k v]]
                                                  [k (if (and (vector? v) (= :all-equal (get-in rule [:when k])))
                                                       (first v) v)]))
                                        binds))))))

(defn- unwrap-fn
  [zloc form]
  (if (and (= :fn (z/tag zloc)) (seq? form) (= 'fn* (first form)))
    (cons form (filter seq? (drop 2 form)))
    [form]))

(defn- ancestors-of
  [env zloc]
  (->> (iterate z/up (z/up zloc))
       (take-while some?)
       (remove #(= :meta (z/tag %)))
       (mapcat (fn [a] (let [s (sexpr a ::no)]
                         (when (not= ::no s) (unwrap-fn a s)))))
       (map #(resolve-head env %))))

(defn- match-rule
  [env {:keys [match either not inside not-inside head-ns]} form zloc]
  (if head-ns
    (when (and (seq? form) (symbol? (first form)) (= head-ns (namespace (first form)))) {})
    (when-let [binds (if either
                       (some #(pat/match % form) either)
                       (pat/match match form))]
      (when (not-any? #(pat/match % form) not)
        (let [ancs (ancestors-of env zloc)]
          (when (clojure.core/not-any? (fn [p] (some #(pat/match p % binds) ancs)) not-inside)
            (if inside
              (some (fn [p] (some (fn [a] (pat/match p a binds)) ancs))
                    (if (vector? inside) inside [inside]))
              binds)))))))

(defn- top-level?
  [zloc]
  (let [u (z/up zloc)] (or (nil? u) (nil? (z/up u)))))

(defn- in-scope?
  [scope {:keys [body top]}]
  (case (or scope :body)
    :body @body
    :top  @top
    :any  true))

(defn- try-rules
  [env rules zloc form]
  (let [form (resolve-head env form)
        scopes {:body (delay (inside-defn? zloc)) :top (delay (top-level? zloc))}]
    (some (fn [rule]
            (when (in-scope? (:scope rule) scopes)
              (when-let [binds (match-rule env rule form zloc)]
                (when (guards-ok? env rule binds (bound-positions zloc binds))
                  [(finding zloc rule binds)]))))
          rules)))

(defn findings
  [env rules zloc]
  (let [env (assoc env :aliases (ns-aliases zloc))]
    (vec (mapcat (fn [c]
                   (let [form (sexpr c ::no)]
                     (when (not= ::no form)
                       (try-rules env rules c form))))
                 (collect zloc (fn [c] (seq? (sexpr c ::no))))))))
