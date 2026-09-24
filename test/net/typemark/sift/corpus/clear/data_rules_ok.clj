(ns data-rules-ok)

;; the file has a static call; without this the (correct) reflection-unwarned fires
(set! *warn-on-reflection* true)

(defn a [t x] (when t x))
(defn b [t x y] (if-not t x y))
(defn c [t x] (when-not t (println x) x))
(defn d [xs] (empty? xs))
(defn e [xs] (seq xs))
(defn f [xs] (clojure.string/join ", " xs))
(defn g [xs] (some even? xs))
(defn h [k] (case k :a 1 :b 2 3))
(defn i [s] (try (Integer/parseInt s) (catch NumberFormatException e (throw (ex-info "bad" {:s s} e)))))
(defn k [x y] (if x y (or x y)))
(defn m [k] (cond (= k :a) 1 (= j :b) 2 :else 3))
(defn n [a b x] (swap! a (fn [v] (conj v (count @b) x))))
(defn o [a] (let [v @a] (swap! a conj v)))

;; a literal JNDI name, a nio temp file, a data format
(defn look [] (javax.naming.InitialContext/doLookup "java:comp/env"))
(defn t [] (java.nio.file.Files/createTempFile "a" "b" (into-array java.nio.file.attribute.FileAttribute [])))
