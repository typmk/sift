(ns net.typemark.sift.host
  (:require [net.typemark.sift.portable.host :as host]
            [clojure.string :as str]
            [net.typemark.sift.zip :refer [children peel list-op op-name head-name
                                           collect inside-defn? pos-of sexpr token-name]]
            [rewrite-clj.zip :as z]))

(defn catch-all-swallow [zloc]
  (for [c (collect zloc #(= "catch" (head-name %)))
        :when (inside-defn? c)
        :let [hit (host/catch-all-swallow (sexpr c ::no))]
        :when hit
        :let [[line col] (or (pos-of c) [nil nil])]]
    (assoc hit :line line :column col)))

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

(defn mutable-escape [zloc]
  (for [d (collect zloc #(contains? #{"defn" "defn-"} (head-name %)))
        :let [lf (last-form d)]
        :when (and lf (mutable-form? lf))
        :let [[line col] (or (pos-of lf) [nil nil])
              nm (some-> (children (peel d)) second token-name)]]
    {:line line :column col
     :symbol (some-> nm symbol)
     :message (str (or nm "fn") " returns a host mutable; callers will assume a value")
     :applicability :unspecified}))
