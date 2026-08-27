(ns com.typemark.sift.zip
  "The zipper vocabulary every shape rule reads the tree with. One copy.

  Four rules each carried their own `children`, `peel`, `list-op`,
  `inside-defn?`, `collect` and `two-binds`, written from the first — the
  same drift `forms.cljc` exists to stop for the node-stream rules. A rule
  is its matcher and its counterpart; what a list's head is, what `^meta`
  hides, and which forms are data belong here.

  Positions are rewrite-clj's, 1-based, from `{:track-position? true}`; a
  zipper made without it answers nil to `pos-of` and every rule still runs."
  (:require [rewrite-clj.zip :as z]))

(defn children
  "Every child location of a branch, left to right."
  [zloc]
  (loop [c (z/down zloc) acc []]
    (if c (recur (z/right c) (conj acc c)) acc)))

(defn peel
  "`^:places/allow acc` is a :meta node wrapping the form. Walk to the form."
  [zloc]
  (loop [c zloc]
    (if (and c (= :meta (z/tag c)))
      (recur (last (children c)))
      c)))

(defn sexpr
  "The form at `zloc`, or `fallback` when it does not read — `#js`, a
  reader conditional, a regex under cljs."
  ([zloc] (sexpr zloc nil))
  ([zloc fallback]
   (try (z/sexpr zloc) (catch #?(:clj Exception :cljs :default) _ fallback))))

(defn pos-of
  "[row col] of a location, or nil."
  [zloc]
  (try (z/position zloc) (catch #?(:clj Exception :cljs :default) _ nil)))

(defn same-form?
  "Two locations at the same position are the same form."
  [a b]
  (and a b (= (pos-of a) (pos-of b))))

(defn list-op
  "The head form of a `()` list, seen through `^meta`, or nil."
  [zloc]
  (let [c (peel zloc)]
    (when (and c (z/list? c))
      (when-let [h (z/down c)]
        (sexpr h)))))

(defn op-name
  "The unqualified name of a head symbol, or nil."
  [sym]
  (when (symbol? sym) (name sym)))

(defn head-name
  "`(list-op zloc)` as a name, or nil."
  [zloc]
  (op-name (list-op zloc)))

(defn token-name
  "The name of a symbol token, seen through `^meta`, or nil."
  [zloc]
  (let [c (peel zloc)]
    (when (and c (= :token (z/tag c)))
      (let [s (sexpr c)]
        (when (symbol? s) (name s))))))

(defn call?
  "A sexpr that is `(n …)` for the unqualified name `n`."
  [form n]
  (and (seq? form) (symbol? (first form)) (= n (name (first form)))))

(def defining-forms
  "Heads under which a rule looks — a top-level `(let …)` is data being
  built, not a function being written."
  #{"defn" "defn-" "fn" "fn*" "defmacro" "defmethod"})

(defn inside-defn?
  "Is some ancestor a function-defining form?"
  [zloc]
  (loop [c (z/up zloc)]
    (cond
      (nil? c) false
      (contains? defining-forms (head-name c)) true
      :else (recur (z/up c)))))

(defn opaque?
  "`#_`, `'…` and `(comment …)`: children are not runtime code."
  [zloc]
  (or (= :uneval (z/tag zloc))
      (= :quote (z/tag zloc))
      (contains? #{"comment" "quote"} (head-name zloc))))

(defn collect
  "Every location under `zloc` (itself included) satisfying `pred`, in
  document order, never descending into opaque forms."
  [zloc pred]
  (let [acc (volatile! [])]
    (letfn [(w [c]
              (when (pred c) (vswap! acc conj c))
              (when-not (opaque? c)
                (doseq [k (children c)] (w k))))]
      (w zloc)
      @acc)))

(defn lists-headed
  "Every `(n …)` list under `zloc` whose head name is in `names`."
  [zloc names]
  (collect zloc (fn [c] (contains? names (head-name c)))))

(def binding-forms
  #{"let" "let*" "loop" "doseq" "for" "dotimes" "binding"})

(defn binder-vec
  "The binding vector of a binding form, or nil."
  [zloc]
  (when (contains? binding-forms (head-name zloc))
    (when-let [v (z/right (z/down (peel zloc)))]
      (when (z/vector? v) v))))

(defn vec-pairs
  "A flat binding vector as [[lhs-zloc rhs-zloc] …]."
  [vz]
  (loop [c (z/down vz) acc []]
    (if (nil? c)
      acc
      (if-let [rhs (z/right c)]
        (recur (z/right rhs) (conj acc [c rhs]))
        acc))))

(defn two-binds
  "Exactly two bindings, both readable, as [[sym init] [sym init]] sexprs;
  nil for anything else. The shape `loop-as-map` and `loop-as-reduce`
  match against."
  [zloc]
  (when-let [vz (binder-vec zloc)]
    (let [pairs (mapv (fn [[l r]] [(sexpr (peel l) ::no) (sexpr r ::no)]) (vec-pairs vz))]
      (when (and (= 2 (count pairs))
                 (every? #(and (symbol? (first %)) (not= ::no (second %))) pairs))
        pairs))))

(defn single-body
  "The one body form of a binding form as a sexpr, or nil when there are
  zero or several."
  [zloc]
  (when-let [d (z/down (peel zloc))]
    (when-let [vz (z/right d)]
      (when-let [body (z/right vz)]
        (when (nil? (z/right body))
          (sexpr body))))))
