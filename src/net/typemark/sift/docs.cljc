(ns net.typemark.sift.docs
  (:require [clojure.string :as str]
            [net.typemark.sift.zip :refer [children collect head-name peel sexpr token-name]]
            [rewrite-clj.zip :as z]))

(def ^:private var-heads #{"defn" "defn-" "defmacro" "defmulti" "def" "defonce" "defprotocol"})

(defn- span [zloc]
  (let [{:keys [row col end-row end-col]} (meta (z/node zloc))]
    {:line row :column col :end-line end-row :end-column end-col}))

(defn- string-at? [zloc]
  (and zloc (#{:token :multi-line} (z/tag zloc)) (string? (sexpr zloc))))

(defn- symbols-in [form]
  (filter symbol? (tree-seq coll? seq form)))

(defn- params-of
  [forms]
  (let [argvs (concat (filter vector? forms)
                      (keep #(when (and (seq? %) (vector? (first %))) (first %)) forms))]
    (into #{} (comp (mapcat symbols-in) (map name) (remove #{"&" "_"})) argvs)))

(defn- words-of
  [forms]
  (into #{}
        (comp (mapcat #(tree-seq coll? seq %))
              (keep #(cond (symbol? %) (name %) (keyword? %) (name %)))
              (mapcat #(str/split (str/lower-case %) #"[-_/.?!*<>=+']+"))
              (remove str/blank?))
        forms))

(defn- var-doc
  [c]
  (let [[_ nm doc & more :as kids] (children (peel c))
        h (head-name c)]
    (when (and (string-at? doc)
               (if (#{"def" "defonce"} h) (= 4 (count kids)) (seq more)))
      (let [rest-forms (keep #(sexpr % nil) more)]
        (assoc (span doc)
               :kind :var
               :name (token-name nm)
               :text (sexpr doc)
               :params (params-of rest-forms)
               :body (words-of rest-forms))))))

(defn- protocol-method-docs
  [c]
  (for [m (drop 2 (children (peel c)))
        :let [ks (children m)]
        :when (and (z/list? m) (> (count ks) 2) (string-at? (last ks)))]
    (assoc (span (last ks))
           :kind :var
           :name (token-name (first ks))
           :text (sexpr (last ks))
           :params (params-of (keep #(sexpr % nil) (butlast (rest ks))))
           :body #{})))

(defn- ns-doc
  [c]
  (let [[_ nm doc] (children (peel c))]
    (if (string-at? doc)
      (assoc (span doc) :kind :ns :name (token-name nm) :text (sexpr doc))
      (assoc (span c) :kind :ns :name (token-name nm) :text nil))))

(defn extract
  [zloc]
  (when zloc
    (vec (mapcat (fn [c]
                   (let [h (head-name c)]
                     (cond
                       (= "ns" h) [(ns-doc c)]
                       (= "defprotocol" h) (keep identity (cons (var-doc c) (protocol-method-docs c)))
                       :else (keep identity [(var-doc c)]))))
                 (collect zloc #(contains? (conj var-heads "ns") (head-name %)))))))
