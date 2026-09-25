(ns net.typemark.sift.zip
  (:require [rewrite-clj.zip :as z]))

(defn children
  [zloc]
  (loop [c (z/down zloc) acc []]
    (if c (recur (z/right c) (conj acc c)) acc)))

(defn peel
  [zloc]
  (loop [c zloc]
    (if (and c (= :meta (z/tag c)))
      (recur (last (children c)))
      c)))

(defn sexpr
  ([zloc] (sexpr zloc nil))
  ([zloc fallback]
   (try (z/sexpr zloc) (catch #?(:clj Exception :cljs :default) _ fallback))))

(defn pos-of
  [zloc]
  (when zloc
    (let [m (meta (z/node zloc))]
      (when (:row m) [(:row m) (:col m)]))))

(defn of-root
  [node]
  (z/of-node* node))

(defn same-form?
  [a b]
  (and a b (= (pos-of a) (pos-of b))))

(defn list-op
  [zloc]
  (let [c (peel zloc)]
    (when (and c (z/list? c))
      (when-let [h (z/down c)]
        (sexpr h)))))

(defn op-name
  [sym]
  (when (symbol? sym) (name sym)))

(defn head-name
  [zloc]
  (op-name (list-op zloc)))

(defn token-name
  [zloc]
  (let [c (peel zloc)]
    (when (and c (= :token (z/tag c)))
      (let [s (sexpr c)]
        (when (symbol? s) (name s))))))

(defn call?
  [form n]
  (and (seq? form) (symbol? (first form)) (= n (name (first form)))))

(def defining-forms
  #{"defn" "defn-" "fn" "fn*" "defmacro" "defmethod"})

(defn inside-defn?
  [zloc]
  (loop [c (z/up zloc)]
    (cond
      (nil? c) false
      (= :fn (z/tag c)) true
      (contains? defining-forms (head-name c)) true
      :else (recur (z/up c)))))

(defn opaque?
  [zloc]
  (or (= :uneval (z/tag zloc))
      (= :quote (z/tag zloc))
      (contains? #{"comment" "quote"} (head-name zloc))))

(defn- walk-collect [zloc pred]
  (let [acc (volatile! [])]
    (letfn [(w [c]
              (when (pred c) (vswap! acc conj c))
              (when-not (opaque? c)
                (doseq [k (children c)] (w k))))]
      (w zloc)
      @acc)))

(def ^:dynamic *locations*
  nil)

(defn locations
  [zloc]
  {:root (z/node zloc) :locs (walk-collect zloc (constantly true))})

(defn collect
  [zloc pred]
  (let [{:keys [root locs]} *locations*]
    (if (and zloc root (identical? root (z/node zloc)))
      (filterv pred locs)
      (walk-collect zloc pred))))

(defn lists-headed
  [zloc names]
  (collect zloc (fn [c] (contains? names (head-name c)))))

(def binding-forms
  #{"let" "let*" "loop" "doseq" "for" "dotimes" "binding"})

(defn binder-vec
  [zloc]
  (when (contains? binding-forms (head-name zloc))
    (when-let [v (z/right (z/down (peel zloc)))]
      (when (z/vector? v) v))))

(defn vec-pairs
  [vz]
  (loop [c (z/down vz) acc []]
    (if (nil? c)
      acc
      (if-let [rhs (z/right c)]
        (recur (z/right rhs) (conj acc [c rhs]))
        acc))))

(defn two-binds
  [zloc]
  (when-let [vz (binder-vec zloc)]
    (let [pairs (mapv (fn [[l r]] [(sexpr (peel l) ::no) (sexpr r ::no)]) (vec-pairs vz))]
      (when (and (= 2 (count pairs))
                 (every? #(and (symbol? (first %)) (not= ::no (second %))) pairs))
        pairs))))

(defn single-body
  [zloc]
  (when-let [d (z/down (peel zloc))]
    (when-let [vz (z/right d)]
      (when-let [body (z/right vz)]
        (when (nil? (z/right body))
          (sexpr body))))))
