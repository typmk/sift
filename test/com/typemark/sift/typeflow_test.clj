(ns com.typemark.sift.typeflow-test
  "Every assertion here was first checked against assay's notes — the JVM
  compiler's own reflection and boxed-math warnings — over three corpora:
  sift itself (boxed 44/45), agentia (reflection 1/1) and lume (boxed
  P 0.87 R 0.98, reflection P 0.58 R 0.96). The fixtures are the shapes
  those runs taught; the numbers are in the README."
  (:require [clojure.test :refer [deftest is testing]]
            [com.typemark.sift.typeflow :as tf]))

(defn- kinds [src] (mapv :kind (tf/predictions src "x.clj" :jvm)))
(defn- at [src] (mapv (juxt :kind :column) (tf/predictions src "x.clj" :jvm)))

(deftest boxed-math-is-an-operand-the-compiler-cannot-unbox
  (testing "unhinted params box; hinted and literal operands do not"
    (is (= [:boxed-math :boxed-math] (kinds "(defn f [a b] (+ (* a b) a))"))
        "the + and the *, as assay reports two")
    (is (= [] (kinds "(defn f [^long a ^long b] (+ (* a b) a))")))
    (is (= [] (kinds "(defn f [] (+ 1 2))"))))
  (testing "a cast or a known core fn is primitive"
    (is (= [] (kinds "(defn f [x] (inc (long x)))")))
    (is (= [] (kinds "(defn f [xs] (inc (count xs)))"))))
  (testing "a let binding carries its init's tag"
    (is (= [] (kinds "(defn f [x] (let [n (long x)] (inc n)))")))
    (is (= [:boxed-math] (kinds "(defn f [x] (let [n (first x)] (inc n)))"))))
  (testing "a host member the table knows returns a primitive"
    (is (= [] (kinds "(defn f [] (quot (System/currentTimeMillis) 1000))"))))
  (testing "bit ops warn on a boxed operand but return long — measured"
    (is (= [[:boxed-math 19]] (at "(defn f [x] (pos? (bit-and (parse-long x) 3)))"))
        "the bit-and boxes; pos? over its long does not"))
  (testing "a #() is a form; math inside it is scored at the compiler's column"
    (is (= [[:boxed-math 20]] (at "(defn f [xs] (map #(inc (:n %)) xs))"))
        "column 20 is the ( after the #, where the compiler points")))

(deftest reflection-is-a-receiver-or-overload-the-compiler-cannot-type
  (is (= [:reflection] (kinds "(defn f [s] (.length s))")))
  (is (= [] (kinds "(defn f [^String s] (.length s))")))
  (testing "a static call or constructor yields a known class; a call on it resolves"
    (is (= [] (kinds "(defn f [] (.digest (java.security.MessageDigest/getInstance \"SHA-256\")))")))
    (is (= [:reflection] (kinds "(defn f [t] (.getBytes (minify t) \"UTF-8\"))"))))
  (testing "catch binds the exception's class"
    (is (= [] (kinds "(defn f [] (try 1 (catch Exception e (.getMessage e))))"))))
  (testing "doto and -> thread the receiver; members judge on it, not their first argument"
    (is (= [] (kinds "(defn f [u] (doto (java.util.Properties.) (.setProperty \"a\" u)))")))
    (is (= [[:reflection 27]] (at "(defn f [cfg u] (doto cfg (.setProperty \"a\" u)))"))
        "at the member form, on the unknown threaded receiver"))
  (testing "an overloaded constructor with a boxed or unknown argument reflects — lume, measured"
    (is (= #{:boxed-math :reflection}
           (set (kinds "(defn f [ttl] (Date. (+ (System/currentTimeMillis) ttl)))"))))
    (is (= [] (kinds "(defn f [^String s] (java.util.UUID/fromString s))"))
        "a static with one overload resolves by name and arity")))

(deftest the-js-host-predicts-nothing-yet
  (is (= [] (tf/predictions "(defn f [x] (.foo x))" "x.cljs" :js))
      "648 predictions against 0 Closure warnings on defnet — see hosts.edn"))
