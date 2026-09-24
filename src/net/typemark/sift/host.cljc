(ns net.typemark.sift.host
  (:require [clojure.string :as str]
            [net.typemark.sift.zip :refer [children peel list-op op-name head-name
                                           collect inside-defn? pos-of sexpr
                                           token-name]]
            [rewrite-clj.zip :as z]))

(def rules
  {:catch-all-swallow     {:category :correctness
                           :instruction "Do not turn a host exception into a constant. Return the failure as data — (ex-info …), {:error …}, or nil with the cause logged — or let it propagate."}
   :mutable-escape        {:category :correctness
                           :instruction "Do not return a host mutable. Convert at the boundary — (vec …), (into {} …), (js->clj …) — so callers receive a value."}
   })

(def ^:private catch-all-classes
  #{"Exception" "Throwable" "java.lang.Exception" "java.lang.Throwable"
    ":default" "js/Error" "js/Object" "Object"})

(defn- constant?
  [forms]
  (or (empty? forms)
      (and (= 1 (count forms))
           (let [f (first forms)]
             (or (nil? f) (keyword? f) (string? f) (number? f) (boolean? f)
                 (and (vector? f) (empty? f)) (and (map? f) (empty? f)))))))

(defn- catch-all-swallow [file zloc]
  (for [c (collect zloc #(= "catch" (head-name %)))
        :when (inside-defn? c)
        :let [[_ cls _ & body] (children (peel c))
              cls-text (some-> cls peel z/string)]
        :when (and cls-text (contains? catch-all-classes cls-text))
        :when (constant? (map #(sexpr % ::no) body))
        :let [[line col] (or (pos-of c) [nil nil])]]
    {:rule :catch-all-swallow :file file :line line :column col
     :symbol (symbol cls-text) :shape :catch-all-swallow
     :message (str "catch " cls-text " returns a constant; the host's failure is now a value that looks like success")
     :applicability :unspecified}))

(def ^:private host-mutables
  #{"ArrayList" "LinkedList" "HashMap" "LinkedHashMap" "TreeMap" "HashSet"
    "LinkedHashSet" "TreeSet" "ArrayDeque" "StringBuilder" "StringBuffer"
    "ConcurrentHashMap" "CopyOnWriteArrayList"
    "js/Array" "js/Map" "js/Set" "js/Object" "js/Date"})

(defn- mutable-form?
  [zloc]
  (let [c (peel zloc)]
    (or (and (= :reader-macro (z/tag c))
             (= "js" (some-> (z/down c) z/string)))
        (when (z/list? c)
          (let [h (list-op c) n (op-name h)]
            (or (contains? #{"clj->js" "js-obj" "array" ".toArray"} n)
                (and (= "new" n)
                     (contains? host-mutables (some-> (children c) second peel z/string)))
                (and n (str/ends-with? n ".")
                     (contains? host-mutables (last (str/split (subs n 0 (dec (count n))) #"\."))))
                (and n (str/ends-with? n ".") (contains? host-mutables (subs n 0 (dec (count n)))))))))))

(defn- last-form
  [defn-zloc]
  (let [xs (children (peel defn-zloc))
        last-child (last xs)]
    (when last-child
      (let [c (peel last-child)]
        (if (and (z/list? c) (some-> (z/down c) z/vector?))
          (last (children c))
          c)))))

(defn- mutable-escape [file zloc]
  (for [d (collect zloc #(contains? #{"defn" "defn-"} (head-name %)))
        :let [lf (last-form d)]
        :when (and lf (mutable-form? lf))
        :let [[line col] (or (pos-of lf) [nil nil])
              nm (some-> (children (peel d)) second token-name)]]
    {:rule :mutable-escape :file file :line line :column col
     :symbol (some-> nm symbol) :shape :mutable-escape
     :message (str (or nm "fn") " returns a host mutable; callers will assume a value")
     :applicability :unspecified}))

(defn findings
  [file zloc]
  (-> (vec (catch-all-swallow file zloc))
      (into (mutable-escape file zloc))))
