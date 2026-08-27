(ns com.typemark.sift.cond-case
  "Third raise: a `cond` whose every test is `(= x <literal>)` on one `x` is
  a `case`. Same shape as the other two — a form that says by hand what a
  core form says by name — and the counterpart is mechanical when the
  literals are keywords, numbers, strings or chars: those are exactly what
  `case` dispatches on by value. `(= x foo)` against a VAR is not this shape
  at all — `case` would read `foo` as a literal symbol — so it is no finding.
  nil/true/false are legal case keys that read badly as one, so a cond over
  them is reported without a counterpart.

  Two clauses at least. One `(= x :a)` and an `:else` is an `if`, and saying
  so is a different rule."
  (:require [rewrite-clj.zip :as z]))

(def rule :cond-as-case)

(def instruction
  "Do not compare one value against literals clause by clause. Use case: (case x :a … :b … default). Keep the clause bodies; drop the (= x …) tests.")

(defn- children [zloc]
  (loop [z (z/down zloc) acc []]
    (if z (recur (z/right z) (conj acc z)) acc)))

(defn- peel [zloc]
  (loop [z zloc]
    (if (and z (= :meta (z/tag z)))
      (recur (last (children z)))
      z)))

(defn- list-op [zloc]
  (let [z (peel zloc)]
    (when (and z (z/list? z))
      (when-let [h (z/down z)]
        (try (z/sexpr h) (catch #?(:clj Exception :cljs :default) _ nil))))))

(defn- op-name [sym] (when (symbol? sym) (name sym)))

(defn- inside-defn? [zloc]
  (loop [z (z/up zloc)]
    (cond
      (nil? z) false
      (contains? #{"defn" "defn-" "fn" "fn*" "defmacro" "defmethod"} (op-name (list-op z))) true
      :else (recur (z/up z)))))

(defn- collect [zloc pred]
  (let [acc (volatile! [])]
    (letfn [(w [z]
              (when (pred z) (vswap! acc conj z))
              (when-not (or (= :uneval (z/tag z))
                            (= :quote (z/tag z))
                            (contains? #{"comment" "quote"} (op-name (list-op z))))
                (doseq [c (children z)] (w c))))]
      (w zloc)
      @acc)))

(defn- literal? [v]
  (or (keyword? v) (number? v) (string? v) (char? v) (nil? v) (boolean? v)))

(defn- case-key?
  "What `case` dispatches on by VALUE. nil/true/false are legal keys too but
  read badly as a case; leave those to a human."
  [v]
  (or (keyword? v) (number? v) (string? v) (char? v)))

(defn- eq-test
  "`(= x k)` or `(= k x)` with exactly one literal side -> [x k], else nil."
  [form]
  (when (and (seq? form) (= '= (first form)) (= 3 (count form)))
    (let [[_ a b] form]
      (cond (and (symbol? a) (literal? b)) [a b]
            (and (symbol? b) (literal? a)) [b a]))))

(defn- clauses
  "`(cond t e t e …)` -> [[test-form expr-form] …] as sexprs, or nil if the
  form does not read."
  [cond-zloc]
  (let [xs (rest (children (peel cond-zloc)))
        forms (map (fn [c] (try (z/sexpr c) (catch #?(:clj Exception :cljs :default) _ ::no))) xs)]
    (when (and (even? (count forms)) (not-any? #{::no} forms))
      (partition 2 forms))))

(defn- finding [file zloc]
  (when-let [cs (seq (clauses zloc))]
    (let [else? (contains? #{:else :default} (first (last cs)))
          body (if else? (butlast cs) cs)
          tests (map (comp eq-test first) body)]
      (when (and (>= (count body) 2)
                 (every? some? tests)
                 (apply = (map first tests)))
        (let [x (ffirst tests)
              ks (map second tests)
              mechanical? (every? case-key? ks)
              [line col] (try (z/position zloc) (catch #?(:clj Exception :cljs :default) _ [nil nil]))]
          (cond-> {:rule rule
                   :file file
                   :line line
                   :column col
                   :symbol x
                   :shape :cond-as-case
                   :message (str "cond compares " x " against " (count body) " literals; the counterpart is case")
                   :instruction instruction
                   :applicability (if mechanical? :mechanical :maybe)}
            mechanical?
            (assoc :counterpart
                   (concat (list 'case x)
                           (mapcat (fn [[k [_ expr]]] [k expr]) (map vector ks body))
                           (when else? [(second (last cs))])))))))))

(defn findings
  [file zloc]
  (vec (keep #(when (inside-defn? %) (finding file %))
             (collect zloc (fn [z] (= "cond" (op-name (list-op z))))))))
