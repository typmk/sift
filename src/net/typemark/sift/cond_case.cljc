(ns net.typemark.sift.cond-case
  (:require [net.typemark.sift.zip :refer [children peel list-op op-name inside-defn? collect pos-of]]
            [rewrite-clj.zip :as z]))

(def rule :cond-as-case)

(def instruction
  "Do not compare one value against literals clause by clause. Use case: (case x :a … :b … default). Keep the clause bodies; drop the (= x …) tests.")

(defn- literal? [v]
  (or (keyword? v) (number? v) (string? v) (char? v) (nil? v) (boolean? v)))

(defn- case-key?
  [v]
  (or (keyword? v) (number? v) (string? v) (char? v)))

(defn- eq-test
  [form]
  (when (and (seq? form) (= '= (first form)) (= 3 (count form)))
    (let [[_ a b] form]
      (cond (and (symbol? a) (literal? b)) [a b]
            (and (symbol? b) (literal? a)) [b a]))))

(defn- clauses
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
              [line col] (or (pos-of zloc) [nil nil])]
          (cond-> {:rule rule
                   :file file
                   :line line
                   :column col
                   :symbol x
                   :shape :cond-as-case
                   :message (str "cond compares " x " against " (count body) " literals; the counterpart is case")
                   :instruction instruction
                   :applicability (if mechanical? :machine-applicable :unspecified)}
            mechanical?
            (assoc :counterpart
                   (concat (list 'case x)
                           (mapcat (fn [[k [_ expr]]] [k expr]) (map vector ks body))
                           [(if else? (second (last cs)) nil)]))))))))

(defn findings
  [file zloc]
  (vec (keep #(when (inside-defn? %) (finding file %))
             (collect zloc (fn [z] (= "cond" (op-name (list-op z))))))))
